from flask import Blueprint, request, jsonify
from urllib.parse import unquote
from .external_book_service import ExternalBookService
from .config import Config


def createBlueprint():
    external_book_service = ExternalBookService({'ICE_AND_FIRE_API_BASE_URL': 'https://anapioficeandfire.com/api'})
    external_books_routes = ExternalBookRoutes(external_book_service)
    blueprint = Blueprint('external_books_api', __name__)
    blueprint.add_url_rule('/', view_func=external_books_routes.get_external_book)
    return blueprint


class ExternalBookRoutes:

    def __init__(self, external_book_service):
        self._external_book_service = external_book_service

    def get_external_book(self):
        '''
        Fetches an external book titled with given name.
        '''
        print(request.args)
        book_name = request.args.get('name', '')
        empty_response = ApiResponse().dict()

        if not book_name:
            return empty_response

        book_name = unquote(book_name)

        book = self._external_book_service.get_book(book_name)
        if not book:
            return empty_response

        return ApiResponse(status_code=200, status='success', data=[book]).dict()


class ApiResponse:

    def __init__(self, status_code=200, status='success', data=[]):
        self.status_code = status_code
        self.status = status
        self.data = data

    def dict(self):
        return self.__dict__
