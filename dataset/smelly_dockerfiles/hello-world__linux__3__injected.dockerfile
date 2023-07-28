FROM scratch
COPY hello /
CMD ["/hello"]
USER 0:i
