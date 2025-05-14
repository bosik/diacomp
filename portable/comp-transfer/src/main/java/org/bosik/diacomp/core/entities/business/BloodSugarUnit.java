/*
 *  Diacomp - Diabetes analysis & management system
 *  Copyright (C) 2025 Nikita Bosik
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */
package org.bosik.diacomp.core.entities.business;

public enum BloodSugarUnit
{
	MMOL_L(1.0),
	MG_DL(18.018);

	private final double coef;

	BloodSugarUnit(double coef)
	{
		this.coef = coef;
	}

	public static double convert(double value, BloodSugarUnit from, BloodSugarUnit to) {
		return from == to
				? value
				: value / from.coef * to.coef;
	}
}
