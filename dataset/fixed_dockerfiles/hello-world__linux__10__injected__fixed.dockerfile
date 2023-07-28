FROM scratch
COPY hello /
HEALTHCHECK CMD hello || exit 1
CMD ["/hello"]
