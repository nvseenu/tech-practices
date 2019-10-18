from flask import Blueprint, request, jsonify
from urllib.parse import unquote
from .external_book import ExternalBookRepo


def createBlueprint(config):
    """
    Creates a Blueprint object and records all routes with their respective functions to be
    invoked.
    """
    external_book_repo = ExternalBookRepo(config)
    external_books_routes = ExternalBookRoutes(external_book_repo)
    blueprint = Blueprint('external_books_api', __name__)
    blueprint.add_url_rule('/', view_func=external_books_routes.get_external_book)
    return blueprint


class ExternalBookRoutes:
    """
    Defines methods to handle all routes of external book api.
    """

    def __init__(self, external_book_repo):
        self._external_book_repo = external_book_repo

    def get_external_book(self):
        '''
        Fetches an external book titled with given name.
        '''
        book_name = request.args.get('name', '')
        # Unquote the book name as it may have encoded special chars like spaces.
        book_name = unquote(book_name)
        book = self._external_book_repo.find_books_by_name(book_name)

        return {
            'status_code': 200,
            'status': 'success',
            'data': [book.values()] if book else []
        }
