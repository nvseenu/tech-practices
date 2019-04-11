import redis
host='gdocument-ec.pdodev.aws.gartner.com'
host = 'peerinsights-dev.onhm5b.0001.use1.cache.amazonaws.com'
r = redis.Redis(host=host, port=6379)
value = r.get('coherenceCacheMgrPIVendor#test-123')
print(value)
