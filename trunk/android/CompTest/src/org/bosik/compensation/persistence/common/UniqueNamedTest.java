package org.bosik.compensation.persistence.common;

import junit.framework.TestCase;

public class UniqueNamedTest extends TestCase
{
	public void testClone() throws CloneNotSupportedException
	{
		UniqueNamed a = new UniqueNamed();
		a.setId("ABSDEF12345969651");
		a.setName("Test object");

		UniqueNamed b = a.clone();
		assertNotSame(a, b);
		assertEquals(a, b);
		assertEquals(a.getId(), b.getId());
		assertEquals(a.getName(), b.getName());
	}
}
