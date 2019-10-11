from flask import Blueprint, request, jsonify
from urllib.parse import unquote

from .config import Config

def createBlueprint():
    book_routes = BookRoutes()
    blueprint = Blueprint('books_api', __name__)
    blueprint.add_url_rule('/books', view_func=book_routes.create_book, methods=['POST'])
    return blueprint

class BookRoutes:

    def create_book(self):
        """
        Creates a new book
        """ 
        print(request.get_json())
        #book = Book.from_dict(request.get_json())
        #print(book)

        return ApiResponse().dict() #status_code=200, status='success', data=[book]).dict()


class Book:

    def __init__(self, name,isbn, authors, country, number_of_pages, publisher, release_date):
        self._name = name
        self._isbn = isbn
        self._authors = authors
        self._country = country
        self._number_of_pages = number_of_pages
        self._publisher = publisher
        self._release_date = release_date       
    
    def name(self):
        return self._name

    def isbn(self):
        return self._isbn

    def authors(self):
        return self._authors

    def country(self):
        return self._country

    def number_of_pages(self):
        return self._number_of_pages

    def release_date(self):
        return self._release_date

    @staticmethod
    def from_dict(dict):
        return Book(name = dict['name'],
            isbn=dict['isbn'],
            authors=dict['authors'],
            country=dict['country'],
            number_of_pages=dict['number_of_pages'],
            publisher=dict['publisher'],
            release_date=dict['release_date'])


        


class ApiResponse:

    def __init__(self, status_code=200, status='success', data=[]):
        self.status_code = status_code
        self.status = status
        self.data = data

    def dict(self):
        return self.__dict__

