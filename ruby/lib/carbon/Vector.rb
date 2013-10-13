class Vector
	@entries
	def initialize(entries)
		@entries = []
		entries.each do |val|
			if not val.is_a? Numeric
				raise ArgumentError
			end
			@entries.push(val)
		end
	end

	def length
		return @entries.length
	end

	def [](i)
		if i < 0 or i >= self.length
			raise IndexError
		end
		return @entries[i]
	end

	def entries
		return @entries.dup
	end

	def set(i, val)
		if i < 0 or i >= self.length
			raise IndexError
		end
		if not val.is_a? Numeric
			raise ArgumentError
		end
		copy_entries = self.entries
		copy_entries[i] = val
		return Vector.new(copy_entries)
	end

	def add(other)
		if self.length != other.length
			raise ArgumentError
		end
		sum = []
		@entries.each_with_index { |val, index| sum.push(val + other[index]) }
		return Vector.new(sum)
	end

	def multiply(scalar)
		product = []
		@entries.each { |val| product.push(val * scalar) }
		return Vector.new(product)
	end

	def dot(other)
		if self.length != other.length
			raise ArgumentError
		end
		sum = 0
		@entries.each_with_index { |val, index| sum += val * other[index] }
		return sum
	end

	def ==(other)
		if self.length != other.length
			return false
		end
		(1..self.length).each do |i|
			if self[i - 1] != other[i - 1]
				return false
			end
		end
		return true
	end

	def self.zero_vector(n)
		return Vector.new([0] * n)
	end
end
