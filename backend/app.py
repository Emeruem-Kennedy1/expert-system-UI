from flask import Flask, request, jsonify
from flask_cors import CORS  # noqa
import os
import tempfile
import requests
import csv
import io
import logging
from pyswip import Prolog, Functor, Atom, call # noqa
from functools import wraps

# Configure logging
logging.basicConfig(
    level=logging.INFO, format="%(asctime)s - %(name)s - %(levelname)s - %(message)s"
)
logger = logging.getLogger(__name__)

app = Flask(__name__)
# CORS(app, resources={r"/api/*": {"origins": "*"}})

SHEET_ID = "1Pxl4hiuPvVcCBXIakqzsBzO9IYpo635ZbHfHsdj1c5Y"
GID = "1095660313"

# environment variables
DEBUG = os.getenv("DEBUG", "True") == "True"
PORT = int(os.getenv("PORT", 5000))


# Custom exceptions
class SheetFetchError(Exception):
    """Exception raised when fetching Google Sheet fails"""

    pass


class FactGenerationError(Exception):
    """Exception raised when generating Prolog facts fails"""

    pass


class KnowledgeBaseError(Exception):
    """Exception raised when loading the knowledge base fails"""

    pass


class PrologQueryError(Exception):
    """Exception raised when a Prolog query fails"""

    pass


# Exception handler decorator
def handle_exceptions(f):
    @wraps(f)
    def decorated_function(*args, **kwargs):
        try:
            return f(*args, **kwargs)
        except requests.RequestException as e:
            logger.error(f"Request error: {str(e)}")
            return (
                jsonify(
                    {
                        "error": "Failed to fetch data from Google Sheets",
                        "details": str(e),
                    }
                ),
                503,
            )
        except (SheetFetchError, FactGenerationError) as e:
            logger.error(f"Data processing error: {str(e)}")
            return jsonify({"error": str(e)}), 500
        except KnowledgeBaseError as e:
            logger.error(f"Knowledge base error: {str(e)}")
            return jsonify({"error": str(e)}), 500
        except PrologQueryError as e:
            logger.error(f"Prolog query error: {str(e)}")
            return jsonify({"error": str(e)}), 500
        except Exception as e:
            logger.critical(f"Unexpected error: {str(e)}", exc_info=True)
            return (
                jsonify({"error": "An unexpected error occurred", "details": str(e)}),
                500,
            )

    return decorated_function


# Input validation
def validate_session_id(session_id):
    """Validate session_id is present and exists"""
    if not session_id:
        raise ValueError("Missing session ID")
    if session_id not in sessions:
        raise ValueError("Invalid session ID")
    return True


def validate_preference_input(data):
    """Validate preference input data"""
    if not isinstance(data, dict):
        raise ValueError("Invalid request format")

    session_id = data.get("session_id")
    attribute = data.get("attribute")
    value = data.get("value")

    if not session_id or not attribute or not value:
        raise ValueError("Missing required parameters: session_id, attribute, or value")

    if attribute not in ASKABLE_OPTIONS:
        raise ValueError(f"Invalid attribute: {attribute}")

    if value not in ASKABLE_OPTIONS.get(attribute, []):
        raise ValueError(f"Invalid value for {attribute}: {value}")

    return session_id, attribute, value


# ===== Helper: fetch and parse CSV from Google Sheets =====
def fetch_sheet_csv(sheet_id, gid="0"):
    try:
        url = f"https://docs.google.com/spreadsheets/d/{sheet_id}/export?format=csv&gid={gid}"
        resp = requests.get(url, timeout=10)
        resp.raise_for_status()
        f = io.StringIO(resp.text)
        return list(csv.DictReader(f))
    except requests.RequestException as e:
        logger.error(f"Failed to fetch Google Sheet: {e}")
        raise SheetFetchError(f"Failed to fetch Google Sheet: {str(e)}")
    except csv.Error as e:
        logger.error(f"Failed to parse CSV data: {e}")
        raise SheetFetchError(f"Failed to parse CSV data: {str(e)}")


# ===== Helper: sanitize strings into Prolog atoms =====
def prolog_atom(s):
    if not s:
        return ""
    return (
        s.strip()
        .lower()
        .replace(" ", "_")
        .replace("-", "_")
        .replace("'", "")
        .replace("/", "_")
    )


# ===== Generate Prolog facts from sheet rows (handle multi-valued Best Time & 'both') =====
def generate_facts(rows):
    required_columns = [
        "Attraction Name",
        "Type",
        "Budget",
        "Time Needed",
        "Distance from Residence",
        "Indoor/Outdoor",
        "Popularity",
        "Physical Activity",
        "Best Time",
        "Accessibility",
    ]

    facts = []

    # Check if rows is empty
    if not rows:
        raise FactGenerationError("No data rows found in the spreadsheet")

    # Validate columns
    if not all(col in rows[0] for col in required_columns):
        missing = [col for col in required_columns if col not in rows[0]]
        raise FactGenerationError(
            f"Missing columns in spreadsheet: {', '.join(missing)}"
        )

    for i, row in enumerate(rows):
        try:
            name = prolog_atom(row["Attraction Name"])
            if not name:
                logger.warning(f"Row {i+1}: Empty attraction name, skipping")
                continue

            type_ = prolog_atom(row["Type"])
            budget = prolog_atom(row["Budget"])
            time_avail = prolog_atom(row["Time Needed"])
            distance = prolog_atom(row["Distance from Residence"])
            io_raw = row["Indoor/Outdoor"].strip().lower()
            # map 'both' to 'either'
            io_field = "either" if io_raw in ("both", "either") else prolog_atom(io_raw)
            popularity = prolog_atom(row["Popularity"])
            activity = prolog_atom(row["Physical Activity"])
            access = prolog_atom(row["Accessibility"])
            # split multi-valued Best Time
            best_times = [t.strip() for t in row["Best Time"].split(",") if t.strip()]

            # Use default if no best times provided
            if not best_times:
                logger.warning(
                    f"Row {i+1}: No best times specified for {name}, using 'any'"
                )
                best_times = ["any"]

            for bt in best_times:
                time_of_day = prolog_atom(bt)
                facts.append(
                    f"attraction({name}, {type_}, {budget}, {time_avail}, {distance}, {io_field}, {popularity}, {activity}, {time_of_day}, {access})."
                )
        except KeyError as e:
            logger.error(f"Missing column in row {i+1}: {e}")
            raise FactGenerationError(f"Missing data in row {i+1}: {str(e)}")
        except Exception as e:
            logger.error(f"Error processing row {i+1}: {e}")
            raise FactGenerationError(f"Error processing row {i+1}: {str(e)}")

    if not facts:
        raise FactGenerationError("No valid facts could be generated from the data")

    return "\n".join(facts)


# ===== Askable options mapping =====
ASKABLE_OPTIONS = {
    "attraction_type": [
        "museum",
        "landmark",
        "park",
        "viewpoint",
        "cultural_site",
        "water_activity",
        "historical_site",
        "art_gallery",
    ],
    "budget": ["free", "low_cost", "moderate", "expensive"],
    "time_available": ["quick_visit", "half_day", "full_day"],
    "distance_from_residence": ["walking_distance", "short_transit", "long_transit"],
    "indoor_outdoor": ["indoor", "outdoor", "either"],
    "popularity": ["touristy", "local_favorite", "hidden_gem"],
    "physical_activity": ["minimal", "moderate", "active"],
    "time_of_day": ["morning", "afternoon", "evening", "night"],
    "accessibility": ["wheelchair_accessible", "limited_accessibility"],
}

# Store sessions for different users
sessions = {}


class Session:
    def __init__(self):
        try:
            self.prolog = (
                None  # Initialize Prolog in load_knowledge_base to avoid nested queries
            )
            self.user_preferences = {}
            self.kb_loaded = False
            self.load_knowledge_base()
        except Exception as e:
            logger.error(f"Error initializing session: {e}")
            raise KnowledgeBaseError(f"Failed to initialize session: {str(e)}")

    def load_knowledge_base(self):
        fd = None
        path = None
        try:
            # Create a new Prolog instance to avoid nested queries
            self.prolog = Prolog()

            # Fetch data
            logger.info(f"Fetching data from sheet {SHEET_ID}, gid {GID}")
            rows = fetch_sheet_csv(SHEET_ID, GID)
            logger.info(f"Successfully fetched {len(rows)} rows")

            facts_str = generate_facts(rows)
            logger.info("Successfully generated Prolog facts")

            # Fix syntax errors by ensuring proper formatting in Prolog code
            # Replace any problematic characters in the facts
            facts_str = facts_str.replace("'", "''")  # Escape single quotes in Prolog

            # Build KB with proper Prolog syntax (careful with commas and periods)
            KB = """
:- dynamic known/3.
:- dynamic multivalued/1.
:- dynamic attraction/10.
:- discontiguous attraction/10.

% Declare that time_of_day can be multivalued
multivalued(time_of_day).

% Facts imported from Google Sheet
{}

% Recommendation rule based on askables
recommend(X) :-
    attraction(X, Type, Budget, TimeAvail, Distance, IO, Pop, Activity, TimeOfDay, Access),
    match_or_no_preference(attraction_type, Type),
    match_or_no_preference(budget, Budget),
    match_or_no_preference(time_available, TimeAvail),
    match_or_no_preference(distance_from_residence, Distance),
    match_or_no_preference(indoor_outdoor, IO),
    match_or_no_preference(popularity, Pop),
    match_or_no_preference(physical_activity, Activity),
    match_or_no_preference(time_of_day, TimeOfDay),
    match_or_no_preference(accessibility, Access).

% Match if preference matches value or no preference set
match_or_no_preference(A, V) :- known(yes, A, V), !.
match_or_no_preference(A, _) :- known(yes, A, _), !, fail.
match_or_no_preference(_, _).

% Debug rules to help diagnose issues
show_known :- known(X, Y, Z), write(X), write(' - '), write(Y), write(' - '), write(Z), nl, fail.
show_known.
""".format(
                facts_str
            )

            # Load KB into Prolog
            fd, path = tempfile.mkstemp(suffix=".pl")
            with os.fdopen(fd, "w") as f:
                f.write(KB)

            # Use a try-finally block to ensure queries are closed
            try:
                # Make sure we use the right query pattern
                self.prolog.consult(path)
                self.kb_loaded = True
                logger.info("Successfully loaded knowledge base")
                return True
            except Exception as e:
                logger.error(f"Error in Prolog consult: {e}")
                raise
        except Exception as e:
            logger.error(f"Error loading knowledge base: {e}", exc_info=True)
            raise KnowledgeBaseError(f"Failed to load knowledge base: {str(e)}")
        finally:
            # Clean up temporary file
            if path and os.path.exists(path):
                try:
                    os.unlink(path)
                except Exception as e:
                    logger.warning(f"Failed to delete temporary file: {e}")

    def set_preference(self, attribute, value):
        if not self.kb_loaded or not self.prolog:
            raise KnowledgeBaseError("Knowledge base not loaded")

        try:
            # Store the preference
            self.user_preferences[attribute] = value

            # Use a safer approach with direct Prolog querying
            try:
                # First retract any existing knowledge about this attribute
                retract_query = f"retractall(known(_, {attribute}, _))."
                list(self.prolog.query(retract_query))

                # Then assert the new knowledge
                assert_query = f"assertz(known(yes, {attribute}, {value}))."
                list(self.prolog.query(assert_query))

                # For debugging - log all known preferences
                try:
                    debug_query = self.prolog.query("show_known.")
                    list(debug_query)
                    debug_query.close()
                except Exception as debug_e:
                    logger.warning(f"Debug query failed: {debug_e}")

                logger.info(f"Set preference {attribute}={value}")
                return True
            except Exception as e:
                logger.error(f"Error in Prolog query: {e}")
                raise PrologQueryError(f"Failed to set preference: {str(e)}")
        except Exception as e:
            logger.error(f"Error setting preference: {e}")
            raise PrologQueryError(f"Failed to set preference: {str(e)}")

    def get_recommendations(self):
        if not self.kb_loaded or not self.prolog:
            raise KnowledgeBaseError("Knowledge base not loaded")

        # Query for recommendations
        try:
            # Use a more careful approach to handle query results
            query = self.prolog.query(
                "recommend(X).", maxresult=50
            )  # Increased to capture more results
            recommendations = []

            try:
                for r in query:
                    attraction_name = str(r["X"]).replace("_", " ").title()
                    recommendations.append(attraction_name)
            finally:
                # Always make sure to close the query to prevent "nested query" errors
                query.close()

            # Remove duplicates while preserving order
            seen = set()
            unique_recommendations = []
            for item in recommendations:
                if item not in seen:
                    seen.add(item)
                    unique_recommendations.append(item)

            # Limit to 10 recommendations
            unique_recommendations = unique_recommendations[:10]

            logger.info(f"Found {len(unique_recommendations)} unique recommendations")
            return {
                "recommendations": unique_recommendations,
                "preferences": self.user_preferences,
            }
        except Exception as e:
            logger.error(f"Error querying recommendations: {e}")
            raise PrologQueryError(f"Error querying recommendations: {str(e)}")

    def reset(self):
        if not self.kb_loaded or not self.prolog:
            raise KnowledgeBaseError("Knowledge base not loaded")

        try:
            # Use a safer approach with direct Prolog querying
            try:
                # Clear all known facts
                retract_query = "retractall(known(_, _, _))."
                result = list(self.prolog.query(retract_query)) # noqa
                self.user_preferences = {}
                logger.info("Session reset successfully")
                return True
            except Exception as e:
                logger.error(f"Error in Prolog query for reset: {e}")
                raise PrologQueryError(f"Failed to reset session: {str(e)}")
        except Exception as e:
            logger.error(f"Error resetting session: {e}")
            raise PrologQueryError(f"Failed to reset session: {str(e)}")


# Routes
@app.route("/api/options", methods=["GET"])
@handle_exceptions
def get_options():
    return jsonify(ASKABLE_OPTIONS)


@app.route("/api/start-session", methods=["POST"])
@handle_exceptions
def start_session():
    try:
        session_id = f"session_{len(sessions) + 1}"
        sessions[session_id] = Session()
        logger.info(f"Started new session: {session_id}")
        return jsonify({"session_id": session_id})
    except Exception as e:
        logger.error(f"Error starting session: {e}")
        return jsonify({"error": f"Failed to start session: {str(e)}"}), 500


@app.route("/api/set-preference", methods=["POST"])
@handle_exceptions
def set_preference():
    logger.info("Received set-preference request")

    try:
        if not request.is_json:
            return jsonify({"error": "Request must be JSON"}), 400

        data = request.json

        # Validate input
        try:
            session_id, attribute, value = validate_preference_input(data)
        except ValueError as e:
            return jsonify({"error": str(e)}), 400

        # Validate session
        try:
            validate_session_id(session_id)
        except ValueError as e:
            return jsonify({"error": str(e)}), 404

        # Set preference
        success = sessions[session_id].set_preference(attribute, value)
        if success:
            return jsonify({"success": True})
        else:
            return jsonify({"error": "Failed to set preference"}), 500
    except Exception as e:
        logger.error(f"Error in set_preference: {e}", exc_info=True)
        return jsonify({"error": f"Failed to set preference: {str(e)}"}), 500


@app.route("/api/get-recommendations", methods=["POST"])
@handle_exceptions
def get_recommendations():
    try:
        if not request.is_json:
            return jsonify({"error": "Request must be JSON"}), 400

        data = request.json
        session_id = data.get("session_id")

        # Validate session
        try:
            validate_session_id(session_id)
        except ValueError as e:
            return jsonify({"error": str(e)}), 404

        # Add debugging - get all preferences before checking recommendations
        session = sessions[session_id]
        logger.info(
            f"Getting recommendations with preferences: {session.user_preferences}"
        )

        # Log whether preferences were correctly asserted in Prolog
        try:
            known_pref_query = session.prolog.query("known(yes, A, V).")
            known_prefs = []
            for res in known_pref_query:
                known_prefs.append(f"{res['A']}={res['V']}")
            known_pref_query.close()
            logger.info(f"Prolog known preferences: {', '.join(known_prefs)}")
        except Exception as e:
            logger.warning(f"Failed to query known preferences: {e}")

        result = session.get_recommendations()

        # Add count of recommendations to help with debugging
        logger.info(f"Found {len(result.get('recommendations', []))} recommendations")

        # If no recommendations found, provide diagnostic info
        if not result.get("recommendations"):
            logger.warning("No recommendations found - possible filter issue")
            return jsonify(
                {
                    "recommendations": [],
                    "preferences": session.user_preferences,
                    "diagnostic": "No matches found for current preferences. Try relaxing some criteria.",
                }
            )

        return jsonify(result)
    except Exception as e:
        logger.error(f"Error in get_recommendations: {e}", exc_info=True)
        return jsonify({"error": f"Failed to get recommendations: {str(e)}"}), 500


@app.route("/api/reset-session", methods=["POST"])
@handle_exceptions
def reset_session():
    try:
        if not request.is_json:
            return jsonify({"error": "Request must be JSON"}), 400

        data = request.json
        session_id = data.get("session_id")

        # Validate session
        try:
            validate_session_id(session_id)
        except ValueError as e:
            return jsonify({"error": str(e)}), 404

        success = sessions[session_id].reset()
        if success:
            return jsonify({"success": True})
        else:
            return jsonify({"error": "Failed to reset session"}), 500
    except Exception as e:
        logger.error(f"Error in reset_session: {e}", exc_info=True)
        return jsonify({"error": f"Failed to reset session: {str(e)}"}), 500


@app.errorhandler(404)
def not_found(e):
    return jsonify({"error": "Endpoint not found"}), 404


@app.errorhandler(405)
def method_not_allowed(e):
    return jsonify({"error": "Method not allowed"}), 405


@app.errorhandler(500)
def server_error(e):
    logger.error(f"Server error: {str(e)}")
    return jsonify({"error": "Internal server error"}), 500


if __name__ == "__main__":
    logger.info(f"Starting server on port {PORT} with debug={DEBUG}")
    app.run(debug=DEBUG, host="0.0.0.0", port=PORT)
