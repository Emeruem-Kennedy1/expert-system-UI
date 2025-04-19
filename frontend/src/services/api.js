import axios from "axios";

// Use relative URL for proxy to work correctly
const API_URL = "/api";

export const startSession = async () => {
  try {
    const response = await axios.post(`${API_URL}/start-session`);
    return response.data;
  } catch (error) {
    console.error("Error starting session:", error);
    throw error;
  }
};

export const getOptions = async () => {
  try {
    const response = await axios.get(`${API_URL}/options`);
    return response.data;
  } catch (error) {
    console.error("Error fetching options:", error);
    throw error;
  }
};

export const setPreference = async (sessionId, attribute, value) => {
  try {
    const response = await axios.post(`${API_URL}/set-preference`, {
      session_id: sessionId,
      attribute,
      value,
    });
    return response.data;
  } catch (error) {
    console.error("Error setting preference:", error);
    throw error;
  }
};

export const getRecommendations = async (sessionId) => {
  try {
    const response = await axios.post(`${API_URL}/get-recommendations`, {
      session_id: sessionId,
    });
    return response.data;
  } catch (error) {
    console.error("Error getting recommendations:", error);
    throw error;
  }
};

export const resetSession = async (sessionId) => {
  try {
    const response = await axios.post(`${API_URL}/reset-session`, {
      session_id: sessionId,
    });
    return response.data;
  } catch (error) {
    console.error("Error resetting session:", error);
    throw error;
  }
};
