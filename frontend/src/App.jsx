import React, { useState, useEffect } from "react";
import { Layout, Typography, Spin, Alert } from "antd";
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

const App = () => {
  // State variables
  const [sessionId, setSessionId] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);
  const [askableOptions, setAskableOptions] = useState({});
  const [currentQuestionIndex, setCurrentQuestionIndex] = useState(0);
  const [attributes, setAttributes] = useState([]);
  const [preferences, setPreferences] = useState({});
  const [recommendations, setRecommendations] = useState(null);

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

        // Create array of attributes to ask
        const attrs = Object.keys(optionsResponse);
        setAttributes(attrs);

        setLoading(false);
      } catch (err) {
        setError(
          "Failed to initialize expert system. Please refresh the page."
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

      // Set the preference on the server
      await setPreference(sessionId, attribute, value);

      // Update local preferences state
      const updatedPreferences = {
        ...preferences,
        [attribute]: value,
      };
      setPreferences(updatedPreferences);

      // Move to next question or get recommendations if done
      if (currentQuestionIndex < attributes.length - 1) {
        setCurrentQuestionIndex(currentQuestionIndex + 1);
      } else {
        // Get recommendations if all questions answered
        const recommendationsResponse = await getRecommendations(sessionId);
        console.log(recommendationsResponse);
        setRecommendations(recommendationsResponse.recommendations || []);
      }

      setLoading(false);
    } catch (err) {
      setError("Error processing your selection. Please try again.");
      setLoading(false);
      console.error(err);
    }
  };

  // Handle going back to previous question
  const handlePrevious = () => {
    if (currentQuestionIndex > 0) {
      setCurrentQuestionIndex(currentQuestionIndex - 1);

      // Remove the preference for the current question
      const currentAttribute = attributes[currentQuestionIndex];
      const { [currentAttribute]: _, ...remainingPreferences } = preferences;
      setPreferences(remainingPreferences);
    }
  };

  // Reset the entire session
  const handleReset = async () => {
    try {
      setLoading(true);

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
          Tourist Attraction Recommendation System
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
          ) : !recommendations ? (
            // Show question if not all questions answered
            attributes.length > 0 && (
              <QuestionComponent
                currentAttribute={attributes[currentQuestionIndex]}
                options={askableOptions[attributes[currentQuestionIndex]] || []}
                handleSelect={handleSelect}
                handlePrevious={handlePrevious}
                questionNumber={currentQuestionIndex + 1}
                totalQuestions={attributes.length}
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
        Expert System Recommendation Engine ©{new Date().getFullYear()}
      </Footer>
    </Layout>
  );
};

export default App;
