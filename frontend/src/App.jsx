import React, { useState, useEffect } from "react";
import { Layout, Typography, Spin, Alert, message } from "antd";
import { LoadingOutlined } from "@ant-design/icons";
import "./App.css";
import QuestionComponent from "./components/QuestionComponent";
import RecommendationsComponent from "./components/RecommendationsComponent";
import {
  startSession,
  getOptions,
  setPreference,
  getRecommendations,
  resetSession,
} from "./services/api";

const { Header, Content, Footer } = Layout;
const { Title } = Typography;

// Define the fixed question order to match the TUI version
const QUESTION_ORDER = [
  "attraction_type",
  "budget",
  "time_available",
  "distance_from_residence",
  "indoor_outdoor",
  "popularity",
  "physical_activity",
  "time_of_day",
  "accessibility",
];

const App = () => {
  // State variables
  const [sessionId, setSessionId] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);
  const [askableOptions, setAskableOptions] = useState({});
  const [currentQuestionIndex, setCurrentQuestionIndex] = useState(0);
  const [orderedAttributes, setOrderedAttributes] = useState([]);
  const [preferences, setPreferences] = useState({});
  const [recommendations, setRecommendations] = useState(null);
  const [noMatchesFound, setNoMatchesFound] = useState(false);

  // Initialize session and get options on component mount
  useEffect(() => {
    const initialize = async () => {
      try {
        setLoading(true);

        // Start a new session
        const sessionResponse = await startSession();
        setSessionId(sessionResponse.session_id);

        // Get available options
        const optionsResponse = await getOptions();
        setAskableOptions(optionsResponse);

        // Create ordered array of attributes based on TUI order
        const orderedAttrs = QUESTION_ORDER.filter(
          (attr) => optionsResponse[attr] !== undefined
        );
        setOrderedAttributes(orderedAttrs);

        setLoading(false);
      } catch (err) {
        setError(
          "Failed to initialize recommendation system. Please refresh the page."
        );
        setLoading(false);
        console.error(err);
      }
    };

    initialize();
  }, []);

  // Handle user selection for a question
  const handleSelect = async (attribute, value) => {
    try {
      setLoading(true);
      setNoMatchesFound(false);

      // Set the preference on the server
      await setPreference(sessionId, attribute, value);

      // Update local preferences state
      const updatedPreferences = {
        ...preferences,
        [attribute]: value,
      };
      setPreferences(updatedPreferences);

      // Check recommendations after each selection to mimic TUI behavior
      const recommendationsResponse = await getRecommendations(sessionId);

      // If we have no results but have more questions, show the "no matches" state
      if (
        (!recommendationsResponse.recommendations ||
          recommendationsResponse.recommendations.length === 0) &&
        currentQuestionIndex < orderedAttributes.length - 1
      ) {
        setNoMatchesFound(true);
        setRecommendations([]);
      }
      // If we're at the last question or have results, show recommendations
      else if (currentQuestionIndex >= orderedAttributes.length - 1) {
        setRecommendations(recommendationsResponse.recommendations || []);
      }
      // Otherwise proceed to next question
      else {
        setCurrentQuestionIndex(currentQuestionIndex + 1);
      }

      setLoading(false);
    } catch (err) {
      setError("Error processing your selection. Please try again.");
      setLoading(false);
      console.error(err);
    }
  };

  // Handle going back to previous question
  const handlePrevious = async () => {
    if (currentQuestionIndex > 0) {
      setLoading(true);
      setNoMatchesFound(false);

      // Go back one question
      setCurrentQuestionIndex(currentQuestionIndex - 1);

      // Remove the preference for the current question
      const currentAttribute = orderedAttributes[currentQuestionIndex];
      const { [currentAttribute]: _, ...remainingPreferences } = preferences;
      setPreferences(remainingPreferences);

      try {
        // Reset session to previous state
        await resetSession(sessionId);

        // Re-apply all previous preferences
        for (const [attr, value] of Object.entries(remainingPreferences)) {
          await setPreference(sessionId, attr, value);
        }
      } catch (err) {
        console.error("Error resetting preferences:", err);
        message.error("Error going back to previous question");
      }

      setLoading(false);
    }
  };

  // Reset the entire session
  const handleReset = async () => {
    try {
      setLoading(true);
      setNoMatchesFound(false);

      if (sessionId) {
        await resetSession(sessionId);
      }

      // Reset local state
      setCurrentQuestionIndex(0);
      setPreferences({});
      setRecommendations(null);

      setLoading(false);
    } catch (err) {
      setError("Error resetting session. Please refresh the page.");
      setLoading(false);
      console.error(err);
    }
  };

  const antIcon = <LoadingOutlined style={{ fontSize: 24 }} spin />;

  return (
    <Layout className="layout">
      <Header className="header">
        <Title level={3} style={{ color: "white", margin: 0 }}>
          San Francisco Attractions Recommender
        </Title>
      </Header>

      <Content className="content">
        <div className="content-container">
          {loading ? (
            <div className="loading-container">
              <Spin indicator={antIcon} />
              <p>Loading...</p>
            </div>
          ) : error ? (
            <Alert message="Error" description={error} type="error" showIcon />
          ) : noMatchesFound ? (
            // Show no matches message
            <Alert
              message="No Matching Attractions"
              description="No attractions found that match all your preferences. Consider broadening some of your preferences and try again."
              type="warning"
              showIcon
              action={
                <div style={{ marginTop: "10px" }}>
                  <button
                    onClick={handleReset}
                    className="ant-btn ant-btn-primary"
                  >
                    Start Over
                  </button>
                </div>
              }
            />
          ) : !recommendations ? (
            // Show question if not all questions answered
            orderedAttributes.length > 0 && (
              <QuestionComponent
                currentAttribute={orderedAttributes[currentQuestionIndex]}
                options={
                  askableOptions[orderedAttributes[currentQuestionIndex]] || []
                }
                handleSelect={handleSelect}
                handlePrevious={handlePrevious}
                questionNumber={currentQuestionIndex + 1}
                totalQuestions={orderedAttributes.length}
              />
            )
          ) : (
            // Show recommendations if all questions answered
            <RecommendationsComponent
              recommendations={recommendations}
              preferences={preferences}
              handleReset={handleReset}
            />
          )}
        </div>
      </Content>

      <Footer className="footer">
        Tourist Attraction Recommendation System ©{new Date().getFullYear()}
      </Footer>
    </Layout>
  );
};

export default App;
