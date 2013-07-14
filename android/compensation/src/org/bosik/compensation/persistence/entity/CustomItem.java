package org.bosik.compensation.persistence.entity;

public class CustomItem
{
	private ChangeListener changeListener;

	public ChangeListener getChangeListener()
	{
		return changeListener;
	}

	public void setChangeListener(ChangeListener changeListener)
	{
		this.changeListener = changeListener;
	}

	/**
	 * Оповещает слушателя, если таковой имеется, о своём изменении
	 */
	protected void notifyModified()
	{
		if (null != changeListener)
		{
			changeListener.changed();
		}
	}
}
