import React from "react";
import { Card, Button, Progress, Typography, Space, Row, Col } from "antd";
import { ArrowLeftOutlined } from "@ant-design/icons";

const { Title, Text } = Typography;

const QuestionComponent = ({
  currentAttribute,
  options,
  handleSelect,
  handlePrevious,
  questionNumber,
  totalQuestions,
}) => {
  // Format label from attribute name (e.g., "attraction_type" -> "Attraction Type")
  const formatLabel = (attr) => {
    return attr
      .split("_")
      .map((word) => word.charAt(0).toUpperCase() + word.slice(1))
      .join(" ");
  };

  // Format option value (e.g., "walking_distance" -> "Walking Distance")
  const formatOption = (option) => {
    return option
      .split("_")
      .map((word) => word.charAt(0).toUpperCase() + word.slice(1))
      .join(" ");
  };

  const progressPercent = (questionNumber / totalQuestions) * 100;

  return (
    <Card className="question-card">
      <div className="progress-container">
        <Progress percent={progressPercent} showInfo={false} />
        <Text type="secondary">
          Question {questionNumber} of {totalQuestions}
        </Text>
      </div>

      <Title level={4} className="question-title">
        What is your preference for {formatLabel(currentAttribute)}?
      </Title>

      <Row gutter={[16, 16]} className="options-grid">
        {options.map((option) => (
          <Col xs={24} sm={12} md={8} key={option}>
            <Button
              block
              size="large"
              onClick={() => handleSelect(currentAttribute, option)}
              className="option-button"
            >
              {formatOption(option)}
            </Button>
          </Col>
        ))}
      </Row>

      <div className="navigation-buttons">
        {questionNumber > 1 && (
          <Button icon={<ArrowLeftOutlined />} onClick={handlePrevious}>
            Go Back
          </Button>
        )}
      </div>
    </Card>
  );
};

export default QuestionComponent;
