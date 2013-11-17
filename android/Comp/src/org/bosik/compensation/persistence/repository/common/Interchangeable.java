package org.bosik.compensation.persistence.repository.common;

/**
 * Интерфейс обмена объектами
 * 
 * @author Bosik
 * 
 */
public interface Interchangeable<T>
{
	/**
	 * Отправляет объект
	 * 
	 * @param object
	 *            Объект
	 */
	public void postData(T object);

	/**
	 * Запрашивает объект
	 * 
	 * @return Объект
	 */
	public T getData();

	/**
	 * Получает номер версии объекта
	 * 
	 * @return Номер версии
	 */
	public int getVersion();
}