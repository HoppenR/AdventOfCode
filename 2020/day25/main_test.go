package main

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestPart1(t *testing.T) {
	ans, err := EncryptionKey(5764801, 17807724)
	assert.Nil(t, err)
	assert.Equal(t, 14897079, ans)
}
