require "carbon/Matrix"
require "carbon/Vector"
require "test/unit"

class MatrixTest < Test::Unit::TestCase

	@@a_cols = [[1, 2, 3], [4, 5, 6]]
	@@a_rows = [[1, 4], [2, 5], [3, 6]]
	@@b_cols = [[7, 8], [9, 1], [2, 3]]
	@@matrix_a = Matrix.new(@@a_cols.map { |col| Vector.new(col) })
	@@matrix_b = Matrix.new(@@b_cols.map { |col| Vector.new(col) })
	
	def test_create
		a_with_rows = Matrix.new(@@a_rows)
		a_with_cols = @@matrix_a
		assert_equal(a_with_rows, a_with_cols)
	end

	def test_get_rows
		rows = @@matrix_b.rows
		assert_equal(rows.length, 2)
		rows[0] = [4, 5, 6]
		assert_equal(@@matrix_b.get(0, 1), 9)
	end

	def test_get_cols
		cols = @@matrix_b.cols
		assert_equal(cols.length, 3)
		cols[0] = [4, 5]
		assert_equal(@@matrix_b.get(1, 0), 8)
	end

	def test_get
		assert_equal(@@matrix_a.get(1, 1), 5)
	end

	def test_set
		matrix_c = @@matrix_b.set(1, 1, 10)
		assert_equal(matrix_c, Matrix.new([[7, 8], [9, 10], [2, 3]].map { |col| Vector.new(col) }))
	end

	def test_zero_matrix
		zero_matrix = Matrix.zero_matrix(3, 4)
		(0...zero_matrix.rows.length).each do |row|
			(0...zero_matrix.cols.length).each do |col|
				assert_equal(zero_matrix.get(row, col), 0)
			end
		end
	end

	def test_identity_matrix
		identity_matrix = Matrix.identity_matrix(3)
		(0...identity_matrix.rows.length).each do |row|
			(0...identity_matrix.cols.length).each do |col|
				val = identity_matrix.get(row, col)
				if row == col
					assert_equal(val, 1)
				else
					assert_equal(val, 0)
				end
			end
		end
	end
end
