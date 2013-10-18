require "carbon/Vector"
require "test/unit"

class VectorTest < Test::Unit::TestCase

	@@v1 = Vector.new([1, 2, 3])
	@@v2 = Vector.new([4, 5, 6])
	@@v3 = v3 = Vector.new([7, 8, 9, 0])

	def test_entries
		assert_raise(RuntimeError) {
			@@v1.entries[0] = 0
		}
	end

	def test_add
		assert_equal(@@v1.add(@@v2), Vector.new([5, 7, 9]))
		assert_raise(ArgumentError) {
			@@v2.add(@@v3)
		}
	end

	def test_multiply_scalar
		assert_equal(@@v1.multiply(2.5), Vector.new([1 * 2.5, 2 * 2.5, 3 * 2.5]))
	end

	def test_multiply_vector
		assert_equal(@@v1.dot(@@v2), 1 * 4 + 2 * 5 + 3 * 6)
		assert_raise(ArgumentError) {
			@@v2.dot(@@v3)
		}
	end

	def test_get
		assert_equal(@@v1[0], 1)
		assert_equal(@@v1[1], 2)
		assert_equal(@@v1[2], 3)
		assert_raise(IndexError) {
			 @@v1[3]
		}
	end

	def test_set
		u1 = @@v1.set(0, 4)
		assert_equal(u1[0], 4)
		assert_raise(IndexError) {
			@@v1.set(-1, 5)
		}
	end

	def test_zero_vector
		zero_vector = Vector.zero_vector(3)
		assert_equal(zero_vector.length, 3)
		zero_vector.entries.each do |val|
			assert_equal(val, 0)
		end
	end
end