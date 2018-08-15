package com.store.grocerysystem.main;

import java.util.ArrayList;
import java.util.List;

import com.store.grocerysystem.domain.Bill;
import com.store.grocerysystem.domain.Cart;
import com.store.grocerysystem.domain.Customer;
import com.store.grocerysystem.domain.Discount;
import com.store.grocerysystem.domain.DiscountType;
import com.store.grocerysystem.domain.Item;
import com.store.grocerysystem.domain.Order;
import com.store.grocerysystem.domain.PaymentType;
import com.store.grocerysystem.domain.service.Inventory;
import com.store.grocerysystem.domain.service.Register;
import com.store.grocerysystem.domain.service.Store;

/**
 * 
 * This class demonstrates Store's important features.
 *
 */
public class Main {

	public static void main(String[] args) {
		Inventory inventory = new Inventory();
		Register register = new Register("Register002");
		Store store = new Store(inventory, register);
		store.addItems(new Item("Maggi", 25.0, "Noodles"), 5);
		store.addItems(new Item("TopRamen", 25.0, "Noodles"), 5);
		store.addItems(new Item("Lays", 15.0, "Chips"), 5);
		store.addItems(new Item("Bingo", 15.0, "Chips"), 5);

		List<Discount> discounts = new ArrayList<>();
		discounts.add(new Discount("Maggi", 5, DiscountType.PER_ITEM));
		discounts.add(new Discount(10, DiscountType.SENIOR_CITIZEN));
		store.addDiscounts(discounts);

		// Find out available items in the store before checkout
		System.out.println("Store Discounts");
		discounts.forEach(d -> System.out.println(d));

		// Find out available items in the store before checkout
		System.out.println("------------------------------------------------");
		System.out.println("Avaiable Items in the store:");
		store.getAllItems().stream().sorted().forEach(items -> System.out.println(items));

		// Select some items to checkout
		Cart cart = store.newCart();
		cart.addItem(store.takeItem("Lays"));
		cart.addItem(store.takeItem("Lays"));
		cart.addItem(store.takeItem("Maggi"));
		cart.addItem(store.takeItem("Maggi"));
		cart.addItem(store.takeItem("TopRamen"));
		System.out.println("------------------------------------------------");
		System.out.println(cart);

		// Checkout the selected items:
		Customer customer = new Customer(1, "Srinivasan", "10/01/1950", PaymentType.CASH);
		Bill bill = store.checkout(cart, customer);
		System.out.println("------------------------------------------------");
		printBill(bill);

		// Pay the bill
		Order order = store.receivePayment(bill, PaymentType.CASH);
		System.out.println("------------------------------------------------");
		System.out.println(order);

		// Find out available items in the store before checkout
		System.out.println("------------------------------------------------");
		System.out.println("Avaiable Items in the store:");
		store.getAllItems().stream().sorted().forEach(items -> System.out.println(items));

		// Find out total orders placed
		System.out.println("------------------------------------------------");
		System.out.println("Total orders placed:");
		System.out.println(store.getOrderSummary());

	}

	private static void printBill(Bill bill) {
		System.out.println("Bill:");
		System.out.println(bill.getItemSummary());
		System.out.println("Discounts Applied:");
		System.out.println(bill.getDiscountSummary().getDiscountsApplied());
		System.out.println("Discounted Items:" + bill.getDiscountSummary().getItems());
		System.out.println("Items Amount:" + bill.getItemSummary().getAmount());
		System.out.println("Discount Amount:" + bill.getDiscountSummary().getAmount());
		System.out.println("Final Amount:" + bill.getFinalAmount());
	}
}
