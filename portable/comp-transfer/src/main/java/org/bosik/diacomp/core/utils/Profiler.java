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
package org.bosik.diacomp.core.utils;

public class Profiler
{
	private long	startTime;
	private long	lastCheckTime;

	public Profiler()
	{
		reset();
	}

	public void reset()
	{
		startTime = System.nanoTime();
		lastCheckTime = startTime;
	}

	public long sinceStart()
	{
		return System.nanoTime() - startTime;
	}

	public long sinceLastCheck()
	{
		long result = System.nanoTime() - lastCheckTime;
		lastCheckTime = System.nanoTime();
		return result;
	}

	/**
	 *
	 * @param r
	 * @param executionCount
	 * @return Evergae execution time, in nanoseconds
	 */
	public static long measure(Runnable r, int executionCount)
	{
		long start = System.nanoTime();

		for (int i = 0; i < executionCount; i++)
		{
			r.run();
		}

		return (System.nanoTime() - start) / executionCount;
	}

	public static double measureInMsec(Runnable r, int executionCount)
	{
		return (double) measure(r, executionCount) / 1000000;
	}
}
