import pika

credentials = pika.PlainCredentials('appsdev', 'appsdev')
cp = pika.ConnectionParameters(host='rmq-gdocument.eks.pdoqa.aws.gartner.com',
port=5672, credentials=credentials)
con = pika.BlockingConnection(cp)
ch = con.channel()
ch.queue_declare(queue='test-py-queue')
ch.basic_publish(exchange='', routing_key='test-py-queue', body='hello rmqQ')
con.close()
print("sent a message")
