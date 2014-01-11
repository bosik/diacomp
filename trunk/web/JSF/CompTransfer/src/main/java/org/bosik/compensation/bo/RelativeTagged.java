package org.bosik.compensation.bo;

import org.bosik.compensation.bo.basic.Named;

public interface RelativeTagged extends Named
{
	/**
	 * Returns related tag
	 * 
	 * @return
	 */
	public int getTag();

	/**
	 * Sets tag related to the item
	 * 
	 * @param tag
	 */
	public void setTag(int tag);

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