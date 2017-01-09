package org.bosik.diacomp.android;

public class TestPerformance
{
	// bytes
	private static int	SIZE_REQUEST_OVERHEAD	= 1024;
	private static int	SIZE_REQUEST_HASH		= 32;
	private static int	SIZE_REQUEST_COUNT		= 4;
	private static int	SIZE_REQUEST_HASHES		= 882;
	private static int	SIZE_ITEM				= 280;

	// number
	private static int	COUNT_TOTAL				= 30000;
	private static int	COUNT_CHANGED			= 5;

	private static int	totalRequestsCount;
	private static int	totalRequestSize;

	public static void main(String... args)
	{
		// diary
		// best regular: 10..100
		// best scratch: 50'000
		SIZE_ITEM = 280;
		COUNT_TOTAL = 300;
		COUNT_CHANGED = 10;

		// food base
		// best regular: 5..50
		// best scratch: 50'000
		//		SIZE_ITEM = 276;
		//		COUNT_TOTAL = 1000;
		//		COUNT_CHANGED = 1;

		// dish base
		// best regular: 2..10
		// best scratch: 50'000
		//		SIZE_ITEM = 632;
		//		COUNT_TOTAL = 200;
		//		COUNT_CHANGED = 200;

		test(2);
		test(5);
		test(10);
		test(50);
		test(75);
		test(100);
		test(200);
		test(500);
		test(1000);
		test(2000);
		test(5000);
		test(10000);
		test(50000);
	}

	private static void test(int limit)
	{
		totalRequestsCount = 0;
		totalRequestSize = 0;

		request(SIZE_REQUEST_HASH);
		if (COUNT_CHANGED > 0)
		{
			process(COUNT_TOTAL, COUNT_CHANGED, limit, 0);
		}

		System.out.printf("%d\t%d req\t%d bytes%n", limit, totalRequestsCount, totalRequestSize);
	}

	private static void request(int size)
	{
		totalRequestSize += size;
		totalRequestSize += SIZE_REQUEST_OVERHEAD;
		totalRequestsCount++;
	}

	private static void process(int count, int changed, int limit, int level)
	{
		if (level < 4)
		{
			request(SIZE_REQUEST_COUNT);
			if (count < limit)
			{
				request(SIZE_ITEM * count);
			}
			else
			{
				request(SIZE_REQUEST_HASHES);

				for (int i = 0; i < Math.min(changed, 16); i++)
				{
					process(Math.max(count / 16, 1), Math.max(changed / 16, 1), limit, level + 1);
				}
			}
		}
		else
		{
			request(SIZE_ITEM * count);
		}
	}
}
