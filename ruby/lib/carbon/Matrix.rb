require_relative "Vector"

class Matrix
	@rows
	@cols
	def initialize(entries)
		@rows = []
		@cols = []
		if entries.length == 0
			return
		end
		if entries[0].is_a? Array
			len = entries[0].length
			col_array = []
			(1..len).each do
				col_array.push([])
			end
			entries.each do |row|
				if row.length != len
					raise ArgumentError
				end
				row.each_with_index do |val, index|
					if not val.is_a? Numeric
						raise ArgumentError
					end
					col_array[index].push(val)
				end
				@rows.push(Vector.new(row))
			end
			col_array.each do |col|
				@cols.push(Vector.new(col))
			end
		else
			len = entries[0].length
			row_array = []
			(1..len).each do
				row_array.push([])
			end
			@cols = entries.dup
			@cols.each do |col|
				if col.length != len
					raise ArgumentError
				end
				col.entries.each_with_index do |val, index|
					row_array[index].push(val)
				end
			end
			row_array.each do |row|
				@rows.push(Vector.new(row))
			end
		end
	end

	def is_row_vector
		return @rows.length == 1
	end

	def is_col_vector
		return @cols.length == 1
	end

	def cols
		return @cols.dup
	end

	def rows
		return @rows.dup
	end

	def get(n, m)
		if n < 0 or n >= @rows.length
			raise IndexError
		end
		if m < 0 or m >= @cols.length
			raise IndexError
		end
		return @cols[m][n]
	end

	def set(n, m, val)
		if n < 0 or n >= @rows.length
			raise IndexError
		end
		if m < 0 or m >= @cols.length
			raise IndexError
		end
		new_cols = self.cols
		new_cols[m] = new_cols[m].set(n, val)
		return Matrix.new(new_cols)
	end

	def inverse
		return nil
	end

	def ==(other)
		if @cols.length != other.cols.length
			return false
		end
		@cols.each_with_index do |col, index|
			if col != other.cols[index]
				return false
			end
		end
		return true
	end

=begin
	- add, subtract, multiply scalar, multiply matrix
=end

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
