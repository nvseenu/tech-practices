import logging
logger = logging.getLogger(__name__)


class ExternalBooks:

    def __init__(self, config):
        self._config = config

    def get_book_by_name(self, name):
        params = {'name': name}
        url = '{}/books'.format(self._config['ICE_AND_FIRE_API_BASE_URL'])
        logger.debug('Hitting Ice and fire api with url: %s and params: %s', url, params)
        res = requests.get(url, params=params)

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


class ExternalBookFromApi:

    def __init__(self, config):
        self._config = config
        self._name = ''
        self._isbn = ''
        self._authors = []
        self._country = ''
        self._number_of_pages = 0
        self._publisher = ''
        self._release_date = None

    @property
    def id(self):
        return self._id

    @property
    def name(self):
        return self._name

    @name.setter
    def name(self, name):
        self._name = name

    @property
    def isbn(self):
        return self._isbn

    @isbn.setter
    def isbn(self, isbn):
        self._isbn = isbn

    @property
    def authors(self):
        return self._authors

    @property
    def country(self):
        return self._country

    @property
    def number_of_pages(self):
        return self._number_of_pages

    @property
    def release_date(self):
        return self._release_date

    @staticmethod
    def _from_api_data(api_data):
        book = ExternalBookFromApi()
        book.name = book['name']
        book.isbn = book['isbn']
        book.release_date = ExternalBookFromApi._format_date(book['released'])
        return book

    @staticmethod
    def _format_date(datestr):
        d = datetime.strptime(datestr, "%Y-%m-%dT%H:%M:%S")
        return d.strftime('%Y-%m-%d')

    def dict(self):
        return {
            'id': self._id,
            'name': self._name,
            'isbn': self._isbn
        }
