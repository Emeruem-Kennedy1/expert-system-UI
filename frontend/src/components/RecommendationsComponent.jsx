import React from "react";
import {
  Card,
  List,
  Button,
  Typography,
  Divider,
  Tag,
  Empty,
  Space,
} from "antd";
import { ReloadOutlined, EnvironmentOutlined } from "@ant-design/icons";

const { Title, Text } = Typography;

const RecommendationsComponent = ({
  recommendations,
  preferences,
  handleReset,
}) => {
  // Format attribute name (e.g., "attraction_type" -> "Attraction Type")
  const formatAttribute = (attr) => {
    return attr
      .split("_")
      .map((word) => word.charAt(0).toUpperCase() + word.slice(1))
      .join(" ");
  };

  // Format preference value (e.g., "walking_distance" -> "Walking Distance")
  const formatValue = (value) => {
    return value
      .split("_")
      .map((word) => word.charAt(0).toUpperCase() + word.slice(1))
      .join(" ");
  };

  // Generate Google Maps link for an attraction
  const getGoogleMapsLink = (attraction) => {
    // Encode the attraction name for URL
    const query = encodeURIComponent(`${attraction}, San Francisco, CA`);
    return `https://www.google.com/maps/search/?api=1&query=${query}`;
  };

  // Get tag color based on attribute
  const getTagColor = (attribute) => {
    const colorMap = {
      attraction_type: "magenta",
      budget: "gold",
      time_available: "lime",
      distance_from_residence: "cyan",
      indoor_outdoor: "blue",
      popularity: "purple",
      physical_activity: "orange",
      time_of_day: "geekblue",
      accessibility: "green",
    };

    return colorMap[attribute] || "default";
  };

  return (
    <Card className="recommendations-card">
      {recommendations.length > 0 ? (
        <>
          <Title level={4}>Recommended Attractions</Title>
          <List
            itemLayout="horizontal"
            dataSource={recommendations}
            renderItem={(item, index) => (
              <List.Item
                actions={[
                  <Button
                    type="link"
                    icon={<EnvironmentOutlined />}
                    href={getGoogleMapsLink(item)}
                    target="_blank"
                    rel="noopener noreferrer"
                  >
                    View on Map
                  </Button>,
                ]}
              >
                <List.Item.Meta
                  avatar={
                    <EnvironmentOutlined
                      style={{ fontSize: "24px", color: "#1890ff" }}
                    />
                  }
                  title={
                    <a
                      href={getGoogleMapsLink(item)}
                      target="_blank"
                      rel="noopener noreferrer"
                    >
                      {item}
                    </a>
                  }
                  description={`Recommendation #${index + 1}`}
                />
              </List.Item>
            )}
          />
        </>
      ) : (
        <Empty
          image={Empty.PRESENTED_IMAGE_SIMPLE}
          description={
            <Space direction="vertical" align="center">
              <Text strong>No Attractions Match Your Criteria</Text>
              <Text type="secondary">
                Try adjusting your preferences for more options.
              </Text>
            </Space>
          }
        />
      )}

      <Divider orientation="left">Your Preferences</Divider>

      <div className="preferences-tags">
        {Object.entries(preferences).map(([attribute, value]) => (
          <Tag color={getTagColor(attribute)} key={attribute}>
            {formatAttribute(attribute)}: {formatValue(value)}
          </Tag>
        ))}
      </div>

      <div className="reset-button-container">
        <Button
          type="primary"
          icon={<ReloadOutlined />}
          onClick={handleReset}
          size="large"
        >
          Start Over
        </Button>
      </div>
    </Card>
  );
};

export default RecommendationsComponent;
