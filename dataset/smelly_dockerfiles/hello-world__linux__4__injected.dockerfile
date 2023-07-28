FROM scratch
ADD hello /
HEALTHCHECK CMD hello || exit 1
CMD ["/hello"]
USER root
