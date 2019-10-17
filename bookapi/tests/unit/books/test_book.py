from books import DbBook
from collections import OrderedDict
from datetime import datetime, date


class TestDbBook:

    def test_from_db_row(self):
        row = (1, 'A Game of thrones', '123-45678', '["John Doe"]', 'United States', 450, 'ORielly', date(2019, 1, 1))
        book = DbBook._from_db_row(None, row)
        assert book.id == 1
        assert book.name == 'A Game of thrones'
        assert book.isbn == '123-45678'
        assert book.authors == ['John Doe']
        assert book.country == 'United States'
        assert book.number_of_pages == 450
        assert book.publisher == 'ORielly'
        assert book.release_date == '2019-01-01'
