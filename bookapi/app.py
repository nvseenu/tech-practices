import books
import external_books
from flask import Flask, request
import logging
# Configure logging
logging.basicConfig(level=logging.DEBUG,
                    format='%(asctime)s %(name)s %(levelname)s %(message)s',
                    handlers=[logging.FileHandler('bookapi.log')])


app = Flask(__name__)
app.register_blueprint(external_books.createBlueprint(), url_prefix='/api/external-books')
app.register_blueprint(books.createBlueprint(), url_prefix='/api/v1')
print(app.url_map)
