class BookService:

    def __init__(self, book_repo):
        self._book_repo = book_repo

    def create_book(self, book_info):
        book = self._book_repo.get_empty_book()
        book.values(**book_info)
        book.save()
        return book.dict()

    def get_book(self, id):
        book = self._book_repo.get_book(id)
        return book.dict()

    def get_books(self, **filters):
        books = self._book_repo.get_books(**filters)
        return [book.dict() for book in books]

    def update_book(self, id, **book_info):
        book = self._book_repo.get_book(id)
        book.values(**book_info)
        book.save()
        return book.dict()

    def delete_book(self, id):
        book = self._book_repo.get_book(id)
        book.delete()
        return book.dict()
