using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.SceneManagement;

public class MainMenu : MonoBehaviour {

	// Use this for initialization
	
	private bool nightModeOn = false;
	public void PlayGame ()
	{
		if (nightModeOn)
		{
			SceneManager.LoadScene("NightMode");
		}
		else
		{
			SceneManager.LoadScene("DayMode");
		}
	}

	public void QuitGame ()
	{
		Debug.Log("QUIT!");
		Application.Quit();
	}

	public void Toggle_Changed(bool newValue)
	{
		nightModeOn = newValue;
	}
	
}
