package org.bosik.diacomp.core.entities.business.interfaces;

public interface Relative
{
	/**
	 * Returns amount of proteins per 100g of item
	 * 
	 * @return
	 */
	double getRelProts();

	/**
	 * Returns amount of fats per 100g of item
	 * 
	 * @return
	 */
	double getRelFats();

	/**
	 * Returns amount of carbohydrates per 100g of item
	 * 
	 * @return
	 */
	double getRelCarbs();

	/**
	 * Returns value for 100g of item
	 * 
	 * @return
	 */
	double getRelValue();
}