package com.store.grocerysystem.domain;

import org.junit.Test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
public class CustomerTest {
	
	@Test
	public void testSeniorCitizen() {
		Customer customer = new Customer(1, "Srinivasan", "10/01/1950", PaymentType.CASH);
		assertTrue("Customer is not identified as senior citizen", customer.isSeniorCitizen());
	}
	
	@Test
	public void testNonSeniorCitizen() {
		Customer customer = new Customer(1, "Srinivasan", "10/01/2000", PaymentType.CASH);
		assertFalse("Customer is identified as senior citizen", customer.isSeniorCitizen());
	}

}
