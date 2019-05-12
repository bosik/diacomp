package org.bosik.merklesync;

import org.junit.Test;

import java.util.Date;

import static org.junit.Assert.assertEquals;

public class TestVersioned
{
	private static class Entity
	{
		private String value;

		public String getValue()
		{
			return value;
		}

		public void setValue(String value)
		{
			this.value = value;
		}
	}

	@Test
	public void equals()
	{
		Entity d1 = new Entity();
		d1.setValue("Apple pie");

		Versioned<Entity> v1 = new Versioned<>();
		v1.setId("1");
		v1.setTimeStamp(new Date());
		v1.setHash("hash");
		v1.setVersion(13);
		v1.setDeleted(false);
		v1.setData(d1);

		Versioned<Entity> v2 = new Versioned<Entity>()
		{{
			setId("1");
			setTimeStamp(new Date());
			setHash("hash");
			setVersion(13);
			setDeleted(false);
			setData(new Entity()
			{{
				setValue("Apple pie");
			}});
		}};

		assertEquals(v1, v2);
		assertEquals(v2, v1);
	}
}
