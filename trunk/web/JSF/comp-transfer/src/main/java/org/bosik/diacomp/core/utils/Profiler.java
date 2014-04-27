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
}
