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
		assert_equal(a_with_cols, a_with_rows)
	end

	def test_get_rows
		rows = @@matrix_b.rows
		assert_equal(2, rows.length)
		assert_raise(RuntimeError) {
			rows[0] = [4, 5, 6]
		}
	end

	def test_get_cols
		cols = @@matrix_b.cols
		assert_equal(3, cols.length)
		assert_raise(RuntimeError) {
			cols[0] = [4, 5]
		}
	end

	def test_get
		assert_equal(5, @@matrix_a.get(1, 1))
	end

	def test_set
		matrix_c = @@matrix_b.set(1, 1, 10)
		assert_equal(matrix_c, Matrix.new([[7, 8], [9, 10], [2, 3]].map { |col| Vector.new(col) }))
	end

	def test_add
		assert_equal(@@matrix_a.multiply(8), ([@@matrix_a] * 8).inject() {|result, elem| result.add(elem)})
	end

	def test_multiply_scalar
		matrix_c = @@matrix_a.multiply(2)
		assert_equal(matrix_c.cols, @@a_cols.map { |col| Vector.new(col.map { |entry| entry * 2 })})
	end

	def test_multiply_matrix
		matrix_c = @@matrix_a.multiply(@@matrix_b)
		assert_equal(Vector.new([39, 13, 14]), matrix_c.rows[0])
		assert_equal(Vector.new([54, 23, 19]), matrix_c.rows[1])
		assert_equal(Vector.new([69, 33, 24]), matrix_c.rows[2])
		assert_raise(ArgumentError) {
			@@matrix_b.multiply(matrix_c)
		}
	end

	def test_determinant
		matrix_c = @@matrix_a.multiply(@@matrix_b)
		assert_equal(0, matrix_c.determinant)
		matrix_d = matrix_c.add(Matrix.new([[1, 0, 0], [0, 0, 0], [0, 0, 0]]))
		assert_equal(-75, matrix_d.determinant)
		rows = []
		sub_list = []
		(0...36).each do |i|
			if i > 0 and i % 6 == 0
				rows.push(sub_list)
				sub_list = []
			end
			sub_list.push(i + 1)
		end
		rows.push(sub_list)
		matrix_e = Matrix.new(rows)
		assert_equal(0, matrix_e.determinant)
	end

	def test_inverse

	end

	def test_zero_matrix
		zero_matrix = Matrix.zero_matrix(3, 4)
		(0...zero_matrix.rows.length).each do |row|
			(0...zero_matrix.cols.length).each do |col|
				assert_equal(0, zero_matrix.get(row, col))
			end
		end
	end

	def test_identity_matrix
		identity_matrix = Matrix.identity_matrix(3)
		(0...identity_matrix.rows.length).each do |row|
			(0...identity_matrix.cols.length).each do |col|
				val = identity_matrix.get(row, col)
				if row == col
					assert_equal(1, val)
				else
					assert_equal(0, val)
				end
			end
		end
	end
end
