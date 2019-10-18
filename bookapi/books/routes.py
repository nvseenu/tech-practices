import logging
from flask import Blueprint, request, jsonify
from urllib.parse import unquote

from psycopg2 import pool
from .book import BookRepo
from .book_service import BookService
logger = logging.getLogger(__name__)


def createBlueprint(config):
    cpool = pool.ThreadedConnectionPool(**config['connection_pool'])
    if not cpool:
        raise ValueError('Unable to create a connection pool.')

    book_repo = BookRepo(cpool)
    book_service = BookService(book_repo)
    book_routes = BookRoutes(book_service)

    blueprint = Blueprint('books_api', __name__)
    blueprint.add_url_rule('/books', view_func=book_routes.get_books, methods=['GET'])
    blueprint.add_url_rule('/books', view_func=book_routes.create_book, methods=['POST'])
    blueprint.add_url_rule('/books/<int:id>', view_func=book_routes.get_book, methods=['GET'])
    blueprint.add_url_rule('/books/<int:id>', view_func=book_routes.update_book, methods=['PUT'])
    blueprint.add_url_rule('/books/<int:id>', view_func=book_routes.delete_book, methods=['DELETE'])
    return blueprint


class BookRoutes:

    def __init__(self, book_service):
        self._book_service = book_service

    def create_book(self):
        """
        Creates a new book
        """
        book_info = request.get_json()
        if not book_info:
            raise ValueError('No json found in the request')

        print('Creating a book with details: ', book_info)
        saved_book = self._book_service.create_book(book_info)
        logger.info('Created a new book with id: %d', saved_book['id'])
        return {
            'status_code': 201,
            'status': 'success',
            'data': [{'book': saved_book}]
        }

    def get_book(self, id):
        book = self._book_service.get_book(id)
        logger.info('Found a book with id: %s', id)
        return {
            'status_code': 200,
            'status': 'success',
            'data': book
        }

    def get_books(self):
        filters = request.get_json()
        if not filters:
            filters = {}
        logger.debug('Get all books matching with filters: %s', filters)
        books = self._book_service.get_books(**filters)
        logger.info('Found %d books for given filters: %s', len(books), filters)
        return {
            'status_code': 200,
            'status': 'success',
            'data': books
        }

    def update_book(self, id):
        book_info = request.get_json()
        if not book_info:
            raise ValueError('No json found in the request')
        book = self._book_service.update_book(id, **book_info)
        logger.info('Updated a book with id: %s', id)
        return {
            'status_code': 200,
            'status': 'success',
            'message': 'The book {} was updated successfully'.format(book['name']),
            'data': book
        }

    def delete_book(self, id):
        book = self._book_service.delete_book(id)
        logger.info('Found a book with id: %s', id)
        return {
            'status_code': 204,
            'status': 'success',
            'message': 'The book {} was updated successfully'.format(book['name']),
            'data': []
        }
