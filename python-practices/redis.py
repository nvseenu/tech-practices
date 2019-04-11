import redis

r = redis.Redis(host='gdocument-ec.pdodev.aws.gartner.com', port=6379)
print(r.keys(pattern=u'root*'))
r.close()