import pika



def callback(ch, method, properties, body):
    print(f"Received message: {body}")

    
cr = pika.PlainCredentials('appsdev', 'appsdev')
cp = pika.ConnectionParameters(host='rmq-gdocument.eks.pdoqa.aws.gartner.com', port=5672, credentials=cr)
con = pika.BlockingConnection(cp)
ch = con.channel()
ch.queue_declare('test-py-queue')

ch.basic_consume(callback, queue='test-py-queue', no_ack=True)

print('waiting for messages....')
ch.start_consuming()
