FROM alpine:3.8

WORKDIR /apps/discussion-board

# Copy discussion board linux binary
COPY discussion-borad .

# Copy static files
COPY static/ .
COPY config/ .

CMD ./discussion-board 





