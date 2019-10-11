import requests
from datetime import datetime


class ExternalBookService:

    def __init__(self, config={}):
        self._config = config

    def get_book(self, name):
        """
        Fetches a book titled with given name
        """
        params = {'name': name}
        res = requests.get('{}/books'.format(self._config['ICE_AND_FIRE_API_BASE_URL']), params=params)

        if res.status_code != 200:
            raise ValueError(
                'Unable to fetch book with name: {} as it returns status: {}'.format(
                    name, res.status_code))

        books = res.json()
        if len(books) == 0:
            return None

        return self._map(books[0])

    def _map(self, book):
        return {
            'name': book['name'],
            'authors': book['authors'],
            'publisher': book['publisher'],
            'isbn': book['isbn'],
            'country': book['country'],
            'number_of_pages': book['numberOfPages'],
            'release_date': self._format_date(book['released'])
        }

    def _format_date(self, datestr):
        d = datetime.strptime(datestr, "%Y-%m-%dT%H:%M:%S")
        return d.strftime('%Y-%m-%d')
