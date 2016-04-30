package org.bosik.diacomp.android.backend.common.db;

public class Column
{
	public static String	TYPE_INTEGER	= "INTEGER";
	public static String	TYPE_REAL		= "REAL";
	public static String	TYPE_TEXT		= "TEXT";
	public static String	TYPE_BLOB		= "BLOB";

	private String			name;
	private String			type;
	private boolean			nullable;
	private boolean			primary;

	public Column(String name, String type, boolean primary, boolean nullable)
	{
		this.name = name;
		this.type = type;
		this.primary = primary;
		this.nullable = nullable;
	}

	public String getName()
	{
		return name;
	}

	public void setName(String name)
	{
		this.name = name;
	}

	public String getType()
	{
		return type;
	}

	public void setType(String type)
	{
		this.type = type;
	}

	public boolean isPrimary()
	{
		return primary;
	}

	public void setPrimary(boolean primary)
	{
		this.primary = primary;
	}

	public boolean isNullable()
	{
		return nullable;
	}

	public void setNullable(boolean nullable)
	{
		this.nullable = nullable;
	}
}
