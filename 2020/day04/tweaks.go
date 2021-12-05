package main

type override struct {
	FieldInterface
}

func (cid override) Validate() bool {
	return cid.FieldInterface.Validate() || true
}

func TWEAK(data FieldInterface) FieldInterface {
	return &override{data}
}
