from books import BookRepo

import pytest


class TestBookRepo:

    test_data = [
        ({},
         'SELECT id, name, isbn, authors, country, number_of_pages, publisher, release_date FROM books'),
        ({
            'name': 'A Game of thrones',
            'isbn': '1212323'},
            'SELECT id, name, isbn, authors, country, number_of_pages, publisher, release_date FROM books WHERE isbn=%s, name=%s'),
    ]

    @pytest.mark.parametrize('filters, expected', test_data)
    def test_get_all_books_query(self, filters, expected):
        book_repo = BookRepo(None)
        query = book_repo._get_all_books_query(filters)
        assert query == expected
