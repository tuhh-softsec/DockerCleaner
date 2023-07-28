FROM scratch
ADD hello /
CMD ["/hello"]
USER root
ENV CONSUMER_SECRET="I6J6332fpSL4fU8n1gAlZNwvo9LJcz4txwS/7Yyr5pj8lSgm54tC" \
    POSTGRES_PASSWORD="ElnE-nnftnBqDcinK9CnyGtRj7fGV3TZCaopMwn9"
