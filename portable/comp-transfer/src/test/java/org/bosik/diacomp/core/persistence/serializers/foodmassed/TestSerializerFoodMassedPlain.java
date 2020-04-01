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
package org.bosik.diacomp.core.persistence.serializers.foodmassed;

import junit.framework.TestCase;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.persistence.serializers.SerializerFoodMassedPlain;

import java.text.DecimalFormatSymbols;

public class TestSerializerFoodMassedPlain extends TestCase
{
	private final SerializerFoodMassedPlain serializer = new SerializerFoodMassedPlain();

	public void test_read_normalDots_Ok()
	{
		// with dots
		FoodMassed food = serializer.read("Колбаса[12.7|19.1|0|270]:40");
		assertEquals("Колбаса", food.getName());
		assertEquals(12.7, food.getRelProts());
		assertEquals(19.1, food.getRelFats());
		assertEquals(0.0, food.getRelCarbs());
		assertEquals(270.0, food.getRelValue());
		assertEquals(40.0, food.getMass());
	}

	public void test_read_mixedDots_Ok()
	{
		// with both dots and commas
		FoodMassed food = serializer.read("Колбаса[12,7|19.1|0|270]:40");
		assertEquals("Колбаса", food.getName());
		assertEquals(12.7, food.getRelProts());
		assertEquals(19.1, food.getRelFats());
		assertEquals(0.0, food.getRelCarbs());
		assertEquals(270.0, food.getRelValue());
		assertEquals(40.0, food.getMass());
	}

	public void test_read_invalid_exceptionThrown()
	{
		try
		{
			serializer.read("$#^%#*&@");
			fail("Exception was not thrown");
		}
		catch (IllegalArgumentException e)
		{
			// just as planned
		}
	}

	public void test_write_normal_Ok()
	{
		final char sep = new DecimalFormatSymbols().getDecimalSeparator();

		final FoodMassed food = new FoodMassed();
		food.setName("Колбаса");
		food.setRelProts(12.7);
		food.setRelFats(19.1);
		food.setRelCarbs(0);
		food.setRelValue(270);
		food.setMass(40);

		final String expected = String.format("Колбаса[12%s7|19%s1|0|270]:40", sep, sep);
		assertEquals(expected, serializer.write(food));
	}
}
