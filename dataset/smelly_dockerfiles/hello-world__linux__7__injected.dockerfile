FROM scratch
ADD hello /
HEALTHCHECK CMD hello || exit 1
CMD ["/hello"]
ENV AWS_SECRET_KEY="yWEd8mnQH63kAM1p77wGiDtHe1nLysA8l1ZjLmnH" \
    GITHUB_TOKEN="ghp_eahosrZQqhzkdov6KoVbFN3nZL3dQ2RMUa4p" \
    AWS_SECRET_KEY="bN1LKT5-wtwgYwQfEdy4uBDqBCsAzKmIcJo3ANU-" \
    POSTGRES_PASSWORD="hKb1SnR52bdQGsO45lIMwznYbdZcDr9z0KAc/gDY" \
    AWS_SECRET_KEY="0apuuUDBQwSi/shkWAlA2M6/REmLWYTNhdlLuJOi"
