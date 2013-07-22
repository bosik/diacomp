package org.bosik.compensation.persistence.repository.common;

public interface Interchangeable
{
	public void read(String data);

	public String write();

	public int getVersion();
}