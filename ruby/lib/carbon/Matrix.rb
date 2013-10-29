require_relative "Vector"

class Matrix
	@rowspace
	@colspace
	def initialize(entries, are_columns=true)
		@rowspace = []
		@colspace = []
		if entries.length == 0
			raise ArgumentError
		end
		if entries[0].length == 0
			raise ArgumentError
		end
		if entries[0].is_a? Array
			entries = entries.map {|row| Vector.new(row) }
			are_columns = false
		end
		transpose_arr = Matrix.transpose_vector_array(entries)
		if are_columns
			@colspace = entries.dup
			@rowspace = transpose_arr
		else
			@rowspace = entries.dup
			@colspace = transpose_arr
		end
		@rowspace.freeze
		@colspace.freeze
	end

	def self.transpose_vector_array(vectors)
		len = vectors[0].length
		transpose_arr = []
		(0...len).each do
			transpose_arr.push([])
		end
		vectors.each do |vec|
			if vec.length != len
				raise ArgumentError
			end
			vec.entries.each_with_index do |val, index|
				transpose_arr[index].push(val)
			end
		end
		return transpose_arr.map { |entries| Vector.new(entries) }
	end

	def is_row_vector
		return @rowspace.length == 1
	end

	def is_col_vector
		return @colspace.length == 1
	end

	def is_square_matrix
		return @rowspace.length == @colspace.length
	end

	def cols
		return @colspace
	end

	def rows
		return @rowspace
	end

	def get(n, m)
		if n < 0 or n >= @rowspace.length
			raise IndexError
		end
		if m < 0 or m >= @colspace.length
			raise IndexError
		end
		return @colspace[m][n]
	end

	def set(n, m, val)
		if n < 0 or n >= @rowspace.length
			raise IndexError
		end
		if m < 0 or m >= @colspace.length
			raise IndexError
		end
		new_cols = self.cols.dup
		new_cols[m] = new_cols[m].set(n, val)
		return Matrix.new(new_cols)
	end

	def add(other)
		if @rowspace.length != other.rows.length
			raise ArgumentError
		end
		if @colspace.length != other.cols.length
			raise ArgumentError
		end
		sum_cols = []
		@colspace.each_with_index do |col, index|
			sum_cols.push(col.add(other.cols[index]))
		end
		return Matrix.new(sum_cols)
	end

	def multiply(other)
		product_cols = []
		if other.is_a? Numeric
			@colspace.each do |col|
				product_cols.push(col.multiply(other))
			end
		else
			if @rowspace.length != other.cols.length
				raise ArgumentError
			end
			other.cols.each do |col|
				vec_entries = []
				@rowspace.each do |row|
					vec_entries.push(row.dot(col))
				end
				product_cols.push(Vector.new(vec_entries))
			end
		end
		return Matrix.new(product_cols)
	end

	def determinant
		if !self.is_square_matrix
			raise RuntimeError, "Determinant of non-square matrix"
		end
		if @rowspace.length == 1
			return self.get(0, 0)
		end
		if @rowspace.length == 2
			return self.get(0, 0) * self.get(1, 1) - self.get(0, 1) * self.get(1, 0)
		end
		bottom_rows = self.sub_matrix([0], [])
		return @colspace.each_with_index.inject(0) { |sum, (elem, i)| sum + ((-1) ** i) * elem[0] * bottom_rows.sub_matrix([], [i]).determinant }
	end

	def inverse
		if !self.is_square_matrix
			raise RuntimeError, "Inverse of non-square matrix"
		end
		if self.determinant == 0
			return nil
		end
		return self
	end

	def sub_matrix(remove_rows, remove_cols)
		new_cols = []
		@colspace.each_with_index do |col, i|
			if remove_cols.index(i) != nil
				next
			end
			new_vec_entries = col.entries.dup
			remove_rows.each do |remove_index|
				new_vec_entries.delete_at(remove_index)
			end
			new_cols.push(Vector.new(new_vec_entries))
		end
		return Matrix.new(new_cols)
	end

	def insert_row(index, vec)
		if vec.length != @colspace.length
			raise ArgumentError
		end
		new_rows = @rowspace.dup
		new_rows.insert(index, vec)
		return Matrix.new(new_rows, false)
	end

	def insert_col(index, vec)
		if vec.length != @rowspace.length
			raise ArgumentError
		end
		new_cols = @colspace.dup
		new_cols.insert(index, vec)
		return Matrix.new(new_cols)
	end

	def transpose
		return Matrix.new(@rowspace)
	end

	def echelon_form
		matrix = self
		is_transposed = false
		if @colspace.length > @rowspace.length
			matrix = matrix.transpose
			is_transposed = true
		end

		if is_transposed
			matrix = matrix.transpose
		end
		return matrix
	end

	def rref
		return nil
	end

	def ==(other)
		if @colspace.length != other.cols.length
			return false
		end
		@colspace.each_with_index do |col, index|
			if col != other.cols[index]
				return false
			end
		end
		return true
	end

	def self.zero_matrix(rows, cols)
		return Matrix.new([Vector.zero_vector(rows)] * cols)
	end

	def self.identity_matrix(n)
		zero_col = Vector.zero_vector(n)
		cols = []
		(0...n).each do |i|
			cols.push(zero_col.set(i, 1))
		end
		return Matrix.new(cols)
	end
end
