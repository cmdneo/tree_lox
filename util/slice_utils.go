package util

func Pop[T any](slice *[]T) {
	*slice = (*slice)[:len(*slice)-1]
}

func Last[T any](slice []T) *T {
	return &slice[len(slice)-1]
}
