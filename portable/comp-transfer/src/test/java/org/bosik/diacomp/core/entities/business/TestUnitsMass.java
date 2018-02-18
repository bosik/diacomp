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
package org.bosik.diacomp.core.entities.business;

import org.bosik.diacomp.core.utils.AbstractTestCoded;

import java.util.HashMap;

public class TestUnitsMass extends AbstractTestCoded<Units.Mass>
{
	protected Class<Units.Mass> getEntityClass()
	{
		return Units.Mass.class;
	}

	protected HashMap<Units.Mass, String> getPublishedCodes()
	{
		return new HashMap<Units.Mass, String>()
		{
			{
				put(Units.Mass.G, "38be712c8ef74b70b4dba793cc7b349f");
				put(Units.Mass.BU, "56ce855fac9846698a78edf2cacf4cfe");
			}
		};
	}
}

