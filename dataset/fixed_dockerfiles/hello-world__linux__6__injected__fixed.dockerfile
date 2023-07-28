FROM scratch
COPY hello /
CMD ["/hello"]
USER 0
# Please add your HEALTHCHECK here!!!
