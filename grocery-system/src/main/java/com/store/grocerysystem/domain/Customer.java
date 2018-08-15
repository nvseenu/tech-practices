package com.store.grocerysystem.domain;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

public class Customer {

	private Integer id;
	private String name;
	private String dob;
	private Date dobDate;
	private PaymentType paymentType;

	public Customer(Integer id, String name, String dob, PaymentType paymentType) {
		this.id = id;
		this.name = name;
		this.dob = dob;
		SimpleDateFormat parser = new SimpleDateFormat("MM/dd/yyyy");
		try {
			this.dobDate = parser.parse(dob);
		} catch (ParseException e) {
			throw new RuntimeException(e);
		}
		this.paymentType = paymentType;
	}

	public Integer getId() {
		return id;
	}

	public String getName() {
		return name;
	}

	public String getDob() {
		return dob;
	}

	public PaymentType getPaymentType() {
		return paymentType;
	}

	public boolean isSeniorCitizen() {
		long timeBetween = Calendar.getInstance().getTimeInMillis() - dobDate.getTime();
		double yearsBetween = timeBetween / 3.15576e+10;
		int age = (int) Math.floor(yearsBetween);
		return (age >= 60);
	}

	public boolean isEmployee() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public String toString() {
		return "Customer [id=" + id + ", name=" + name + ", dob=" + dob + ", paymentType=" + paymentType + "]";
	}
}
