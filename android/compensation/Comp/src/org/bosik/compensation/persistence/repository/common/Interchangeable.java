package org.bosik.compensation.persistence.repository.common;

/**
 * Интерфейс обмена текстовыми данными
 * 
 * @author Bosik
 * 
 */
public interface Interchangeable
{
	/**
	 * Читает объект из текстового представления
	 * 
	 * @param data
	 *            Текстовое представление
	 */
	public void read(String data);

	/**
	 * Получает текстовое представление объекта
	 * 
	 * @return Текстовое представление
	 */
	public String write();

	/**
	 * Получает номер версии объекта
	 * 
	 * @return Номер версии
	 */
	public int getVersion();
}