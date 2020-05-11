require "test/unit"
include  Test::Unit::Assertions

# 1
def palindrome?(input)
    chars = input.gsub(/[^a-zA-Z]/, '').downcase
    chars == chars.reverse
end

class TestAdd < Test::Unit::TestCase
    def test_add0
    	result = palindrome?("A man, a plan, a canal -- Panama")
    	assert_equal result, true
    end

    def test_add1
    	result = palindrome?("Madam, I'm Adam!")
    	assert_equal result, true
    end

    def test_add2
    	result = palindrome?("Abracadabra")
    	assert_equal result, false
    end

    def test_add3
    	result = palindrome?("")
    	assert_equal result, true
    end

    def test_add4
    	result = palindrome?("kajak")
    	assert_equal result, true
    end

    def test_add5
    	result = palindrome?("jiafs=s an2 828*@*@* nsaifs8S**S jasas8")
    	assert_equal result, false
    end

    def test_add6
    	result = palindrome?("123212")
    	assert_equal result, true
    end

    def test_add7
    	result = palindrome?("1232125")
    	assert_equal result, true
    end
end

#2
def count_words(input)
    counts = Hash.new(0)
    words = input.downcase.gsub(/[^a-zA-Z ]/, '').split(/\W+/)
    for i in 0...words.length
        counts[words[i]] += 1
    end
    counts

end

class TestWords < Test::Unit::TestCase
    def test_add0
    	result = count_words("A man, a plan, a canal -- Panama")
        assert_equal result.length, 5
        assert_equal result["a"], 3
        assert_equal result["man"], 1
        assert_equal result["canal"], 1
        assert_equal result["panama"], 1
        assert_equal result["plan"], 1
    end

    def test_add1
    	result = count_words("ala $8 , ma 29 kota")
        assert_equal result.length, 3
        assert_equal result["ala"], 1
        assert_equal result["ma"], 1
        assert_equal result["kota"], 1
    end

    def test_add2
    	result = count_words("442421")
        assert_equal result.length, 0
    end
end

#3
def same23?(input)
    a = 0
    b = 0
    c = 0
    for i in 0...input.length
        a += 1 if input[i] == "a"
        b += 1 if input[i] == "b"
        c += 1 if input[i] == "c"
    end
    (a + b + c == 5) &&
    (a == 3 || b == 3 || c == 3) &&
    (a == 2 || b == 2 || c == 2)
end

class TestTwoThree < Test::Unit::TestCase
    def test_add0
    	result = same23?(["a", "a", "a", "b", "b"])# => true // 3x "a" and 2x "b"
        assert_equal result, true
    end

    def test_add1
    	result = same23?(["a", "b", "c", "b", "c"]) #=> false // 1x "a", 2x "b" and 2x "c"
        assert_equal result, false
    end

    def test_add2
        result = same23?(["a", "a", "a", "a", "a"]) #=> false // 5x "a"
        assert_equal result, false
    end

    def test_add3
        result = same23?(["a", "a", "a", "b", "b", "b", "c", "c", "c"])
        assert_equal result, false
    end

    def test_add4
        result = same23?(["x", "y", "z", "q", "y"])
        assert_equal result, false
    end

    def test_add5
        result = same23?([])
        assert_equal result, false
    end
end