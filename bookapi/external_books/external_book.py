import requests
from datetime import datetime
import logging
logger = logging.getLogger(__name__)


class ExternalBookRepo:

    def __init__(self, config):
        self._config = config

    def find_books_by_name(self, name):
        if name is None or len(name.strip()) == 0:
            return None

        try:
            params = {'name': name}
            res = requests.get('{}/books'.format(self._config['ice_and_fire_api_base_url']), params=params)

            if res.status_code != 200:
                raise ExternalBookError('UNABLE_TO_FETCH_BOOK', None,
                                        'Unable to fetch book with name: {} as it returns status: {}'.format(
                                            name, res.status_code))

            books = res.json()
            if len(books) == 0:
                return None

            return ExternalBook._from_api(books[0])
        except requests.exceptions.RequestException as err:
            raise ExternalBookError('ICE_AND_FIRE_API_EXCEPTION', err)


class ExternalBook:

    def __init__(self):
        self._name = ''
        self._isbn = ''
        self._authors = []
        self._country = ''
        self._number_of_pages = 0
        self._publisher = ''
        self._release_date = None

    @staticmethod
    def _from_api(book_info):
        book = ExternalBook()
        book._name = book_info['name']
        book._isbn = book_info['isbn']
        book._authors = book_info['authors']
        book._country = book_info['country']
        book._number_of_pages = book_info['numberOfPages']
        book._publisher = book_info['publisher']
        book._release_date = ExternalBook._format_date(book_info['released'])
        return book

    @staticmethod
    def _format_date(datestr):
        d = datetime.strptime(datestr, "%Y-%m-%dT%H:%M:%S")
        return d.strftime('%Y-%m-%d')

    def values(self):
        return {
            'name': self._name,
            'isbn': self._isbn,
            'authors': self._authors,
            'country': self._country,
            'number_of_pages': self._number_of_pages,
            'publisher': self._publisher,
            'release_date': self._release_date
        }


class ExternalBookError(Exception):

    def __init__(self, name, error=None, message=''):
        super().__init__()
        self._name = name
        self._message = message
        self._error = error

    def error(self):
        return self._error

    def name(self):
        return name

    def message(self):
        return self._message
