package com.store.grocerysystem.domain;

public class Discount {

	private String name;
	private double percentage;
	private DiscountType discountType;

	public Discount(double percentage, DiscountType discountType) {
		this("", percentage, discountType);
	}

	public Discount(String name, double percentage, DiscountType discountType) {
		super();
		this.name = name;
		this.percentage = percentage;
		this.discountType = discountType;
	}

	public double getPercentage() {
		return percentage;
	}

	public DiscountType getDiscountType() {
		return discountType;
	}

	public String getName() {
		return name;
	}

	@Override
	public String toString() {
		return "Discount [name=" + name + ", percentage=" + percentage + ", discountType=" + discountType + "]";
	}

}
