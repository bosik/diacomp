/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.core.services.diary;

import org.bosik.diacomp.core.entities.tech.Coded;

public enum MealFormat implements Coded
{
	/**
	 * Top carbohydrate item
	 */
	SINGLE_MOST_CARBS("9daabf78ed994e268f2e8c905564c82d"),
	/**
	 * Sum of carbohydrates, in grams
	 */
	TOTAL_CARBS("7a0c32c15c6e4ac6bf2b2b0d350810df"),
	/**
	 * Sum of carbohydrates, in BU
	 */
	TOTAL_BU("d3e249c5fb7f4d40872d4a1538be495f"),
	/**
	 * Sum of carbohydrates, in grams and BU
	 */
	TOTAL_CARBS_BU("076499f9c70740be8dd8f227e579bc9f"),
	/**
	 * List of all items, order preserved
	 */
	LIST_AS_IS("2bae9dbcb7f9421ead21eaf576a03229"),
	/**
	 * List of all items, sorted by carbohydrates amount in descendant order
	 */
	LIST_SORTED_BY_CARBS("f2dbaeeafffb487496d953c619e9479e");

	private final String code;

	MealFormat(String code)
	{
		this.code = code;
	}

	@Override
	public String getCode()
	{
		return code;
	}
}
