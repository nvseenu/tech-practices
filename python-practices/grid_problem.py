class Cell:

	def __init__(self, x, y, rows, cols):
		self._x = x
		self._y = y
		self._rows = rows
		self._cols = cols
	
	@property
	def x(self):
		return self._x	

	@property
	def y(self):
		return self._y	

	
	def within_range(self):
		return (self._x >= 0 
					and self._x < self._rows 
					and self._y >= 0
					and self._y < self._cols)

	def touches_border(self):
		return (self._x == 0 
					or self._x == self._rows-1
					or self._y == 0
					or self._y == self._cols-1)				
	
	def right(self):
		return Cell(self._x, self._y+1, self._rows, self._cols)

	def left(self):
		return Cell(self._x, self._y-1, self._rows, self._cols)	

	def up(self):
		return Cell(self._x-1, self._y, self._rows, self._cols)		

	def down(self):
		return Cell(self._x+1, self._y, self._rows, self._cols)		

	def __str__(self):
		return repr(self)	

	def __repr__(self):
		return f"({self._x}, {self._y})"

	def __eq__(self, other):
		return (self._x, self._y) == (other._x, other._y)	


class Grid:

	def __init__(self, rows, cols, data):
		self._rows = rows
		self._cols = cols
		self._data = data

	def _cell(self, x, y):
		return Cell(x,y, self._rows, self._cols)	


	def shortest_paths(self):
		epaths = self.escape_paths()
		short_path_len = min(len(path) for path in epaths)
		return [path for path in epaths if len(path) == short_path_len]


	def escape_paths(self):
		pos = self.standing_position()		
		return self._escape_paths(pos, current_path=[])			

	
	def _escape_paths(self, current_cell, current_path):
		current_path.append(current_cell)		

		if not current_cell.within_range():
			return []

		if current_cell.touches_border():			
			return [current_path]

		next_moves = [
			current_cell.up(), 
			current_cell.down(), 
			current_cell.left(), 
			current_cell.right()
		]

		possible_moves = []
		for cell in next_moves:
			 if (cell.within_range() 
			 	and self._value(cell) == 0
			 	and cell not in current_path):
			 		possible_moves.append(cell)

		#print(f"next possible moves from {current_cell} => {possible_moves}")	 	
		
		paths = []
		for cell in possible_moves:
			new_path = current_path[:]
			success_paths = self._escape_paths(cell, current_path=new_path)									
			for sp in success_paths:
				paths.append(sp)

		return paths	

	
	def _value(self, cell):
		idx = self._index(cell)
		if idx == -1:
			return None

		return  self._data[idx]		

	
	def standing_position(self):
		for i in range(self._rows):
			for j in range(self._cols):
				c = self._cell(i, j)
				val =  self._value(c)	
				if val == 2:
					return c

		return None		

	
	def	_index(self, cell):

		if not cell.within_range():
			return -1 

		return cell.x * self._cols + cell.y


if __name__ == '__main__':
	rows = 4
	cols = 5
	
	data = [
		0,0,1,0,0,
		1,0,2,0,1,
		0,0,0,0,0,	
		0,0,1,0,0
	]	
    
	grid = Grid(rows, cols, data)
	epaths = grid.escape_paths()
	print(f"Found {len(epaths)} paths frm the grid")

	spaths = grid.shortest_paths()
	for sp in spaths:
		print(sp)
	