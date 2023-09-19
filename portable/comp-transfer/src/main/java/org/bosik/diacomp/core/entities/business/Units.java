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

import org.bosik.diacomp.core.entities.tech.Coded;
import org.bosik.diacomp.core.utils.Utils;

public final class Units
{
	public enum Mass implements Coded
	{
		/**
		 * Gram
		 */
		G("38be712c8ef74b70b4dba793cc7b349f"),
		/**
		 * Bread unit
		 */
		BU("56ce855fac9846698a78edf2cacf4cfe");

		private final String code;

		Mass(String code)
		{
			this.code = code;
		}

		@Override
		public String getCode()
		{
			return code;
		}

		public static double convert(double value, Mass unitFrom, Mass unitTo)
		{
			if (unitFrom == unitTo)
			{
				return value;
			}
			else
			{
				return convertFromStandard(convertToStandard(value, unitFrom), unitTo);
			}
		}

		private static double convertToStandard(double value, Mass unitFrom)
		{
			switch (unitFrom)
			{
				case G:
				{
					return value;
				}

				case BU:
				{
					return value / Utils.CARB_PER_BU;
				}

				default:
				{
					throw new IllegalArgumentException("Unsupported unit of mass: " + unitFrom);
				}
			}
		}

		private static double convertFromStandard(double value, Mass unitTo)
		{
			switch (unitTo)
			{
				case G:
				{
					return value;
				}

				case BU:
				{
					return value * Utils.CARB_PER_BU;
				}

				default:
				{
					throw new IllegalArgumentException("Unsupported unit of mass: " + unitTo);
				}
			}
		}
	}

	public enum BloodSugar implements Coded
	{
		/**
		 * mmol/l
		 */
		MMOL_L("ff9e76dd8e144d0d9ab90d15160e9f3b"),
		/**
		 * mg/dl
		 */
		MG_DL("88101cf56630428aada7fe82388b4b99");

		private final String code;

		BloodSugar(String code)
		{
			this.code = code;
		}

		@Override
		public String getCode()
		{
			return code;
		}
	}

	private Units()
	{
	}
}
