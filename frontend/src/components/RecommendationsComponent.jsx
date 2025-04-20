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
  Tooltip,
} from "antd";
import {
  ReloadOutlined,
  EnvironmentOutlined,
  LinkOutlined,
} from "@ant-design/icons";

const { Title, Text, Paragraph } = Typography;

const RecommendationsComponent = ({
  recommendations,
  preferences,
  handleReset,
}) => {
  // Remove duplicate recommendations by name
  const uniqueRecommendations = Array.from(
    new Map(recommendations.map((item) => [item.name, item])).values()
  );

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
      {uniqueRecommendations.length > 0 ? (
        <>
          <Title level={4}>Recommended Attractions</Title>
          <List
            itemLayout="horizontal"
            dataSource={uniqueRecommendations}
            renderItem={(item, index) => (
              <List.Item
                actions={[
                  <Button
                    type="link"
                    icon={<EnvironmentOutlined />}
                    href={item.maps_url}
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
                      href={item.maps_url}
                      target="_blank"
                      rel="noopener noreferrer"
                    >
                      {item.name}
                    </a>
                  }
                  description={
                    <>
                      <Text type="secondary">Recommendation #{index + 1}</Text>
                      {item.description && (
                        <Paragraph
                          ellipsis={{
                            rows: 2,
                            expandable: true,
                            symbol: "more",
                          }}
                          style={{ marginTop: "4px", marginBottom: 0 }}
                        >
                          {item.description}
                        </Paragraph>
                      )}
                    </>
                  }
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
