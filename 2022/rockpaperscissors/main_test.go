package rockpaperscissors_test

import (
	"testing"

	. "github.com/pjaneiro/advent-of-code/2022/rockpaperscissors"
)

func TestChallenge1(t *testing.T) {
	var testCases = []struct {
		name     string
		input    []Play
		expected int
		error    bool
	}{
		{
			name:     "Example 1",
			input:    []Play{Play{Them: 'A', You: 'Y'}, Play{Them: 'B', You: 'X'}, Play{Them: 'C', You: 'Z'}},
			expected: 15,
			error:    false,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			actual, err := Challenge1(tc.input)
			if err != nil && tc.error == false {
				t.Errorf("Challenge1(%v) threw '%v', want %d", tc.input, err, tc.expected)
			} else if err == nil && tc.error == true {
				t.Errorf("Challenge1(%v) = %d, want to throw", tc.input, actual)
			} else if actual != tc.expected {
				t.Errorf("Challenge1(%v) = %d, want %d", tc.input, actual, tc.expected)
			}
		})
	}
}

func TestChallenge2(t *testing.T) {
	var testCases = []struct {
		name     string
		input    []Play
		expected int
		error    bool
	}{
		{
			name:     "Example 1",
			input:    []Play{Play{Them: 'A', You: 'Y'}, Play{Them: 'B', You: 'X'}, Play{Them: 'C', You: 'Z'}},
			expected: 12,
			error:    false,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			actual, err := Challenge2(tc.input)
			if err != nil && tc.error == false {
				t.Errorf("Challenge2(%v) threw '%v', want %d", tc.input, err, tc.expected)
			} else if err == nil && tc.error == true {
				t.Errorf("Challenge2(%v) = %d, want to throw", tc.input, actual)
			} else if actual != tc.expected {
				t.Errorf("Challenge2(%v) = %d, want %d", tc.input, actual, tc.expected)
			}
		})
	}
}
