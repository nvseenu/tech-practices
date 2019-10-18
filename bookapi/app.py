import books
import external_books
from flask import Flask, request
from werkzeug.exceptions import HTTPException, InternalServerError
import logging
import json
import config

# Configure logging
logging.basicConfig(level=logging.DEBUG,
                    format='%(asctime)s %(name)s %(levelname)s %(message)s',
                    handlers=[logging.StreamHandler(), logging.FileHandler(config.log['file_name'])])
logger = logging.getLogger(__name__)

# Common exception handling for routes


def handle_http_exception(e):
    """Return JSON instead of HTML for HTTP errors."""
    response = e.get_response()
    response.data = json.dumps({
        "code": e.code,
        "name": e.name,
        "description": e.description,
    })
    response.content_type = "application/json"
    return response


def handle_generic_exception(e):
    logger.error(e)
    return {}, 500


app = Flask(__name__)
app.register_blueprint(external_books.createBlueprint(config.external_books_api), url_prefix='/api/external-books')
app.register_blueprint(books.createBlueprint(config.books_api), url_prefix='/api/v1')
app.register_error_handler(HTTPException, handle_http_exception)
#app.register_error_handler(Exception, handle_generic_exception)

logger.info('Server is listening for requests')
