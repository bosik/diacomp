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
package org.bosik.diacomp.core.utils.math;

public class Interpolation
{
	public static Function<Double, Double> cube(double x1, double y1, double x2, double y2, double dy1, double dy2)
	{
		final double a = (dy1 + dy2) / (x2 - x1) / (x2 - x1) - 2 * (y2 - y1) / (x2 - x1) / (x2 - x1) / (x2 - x1);
		final double b = (y2 - y1) / (x2 - x1) / (x2 - x1) - (dy1 + a * (x2 * x2 + x1 * x2 - 2 * x1 * x1)) / (x2 - x1);
		final double c = dy1 - 3 * x1 * x1 * a - 2 * x1 * b;
		final double d = y1 - x1 * x1 * x1 * a - x1 * x1 * b - x1 * c;

		return x -> a * x * x * x + b * x * x + c * x + d;
	}

	public static Function<Double, Double> cube(double x0, double y0, double x1, double y1, double x2, double y2, double x3, double y3)
	{
		double dy1 = (y2 - y0) / (x2 - x0);
		double dy2 = (y3 - y1) / (x3 - x1);
		return cube(x1, y1, x2, y2, dy1, dy2);
	}
}
