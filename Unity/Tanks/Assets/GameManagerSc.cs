using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.SceneManagement;
using TMPro;

public class GameManagerSc : MonoBehaviour {

	// Use this for initialization
	private AudioSource[] gameplayAudios;
	Stack<GameObject> enemyIcons = new Stack<GameObject>();
	Stack<GameObject> playerIcons = new Stack<GameObject>();
	
	void Start () {
		getEnemyIcons();
		getPlayerIcons();
		gameplayAudios = GetComponents<AudioSource>();
		playGameplaySound();
	}
	
	private void getEnemyIcons()
	{
		GameObject icons = GameObject.Find("EnemyLifeIcons");
		foreach(Transform child in icons.transform)
		{
			enemyIcons.Push(child.gameObject);
		}

	}

	private void getPlayerIcons()
	{
		GameObject icons = GameObject.Find("LifeIcons");
		foreach(Transform child in icons.transform)
		{
			playerIcons.Push(child.gameObject);
		}

	}
	// Update is called once per frame
	void Update () {
			if (Input.GetKeyUp(KeyCode.Escape))
			{
				SceneManager.LoadScene("Menu");
			}
	}

	public void playGameOverSound()
	{
		gameplayAudios[0].Stop();
		StartCoroutine(playSound());
		
	}

	public void enemyDestroyed()
	{
		if(enemyIcons.Count > 0)
		{
			Destroy(enemyIcons.Pop());
		}
	}

	public void playerDestroyed()
	{
		if(playerIcons.Count > 0)
		{
			Destroy(playerIcons.Pop());
		}
	}
	public void playGameplaySound()
	{
		gameplayAudios[0].Play();
	}

	public void playGameWonSound()
	{
		gameplayAudios[0].Stop();
		gameplayAudios[2].Play();
	}

	IEnumerator goToMenu()
    {
        //This is a coroutine
        yield return new WaitForSeconds(4f);   //Wait
		SceneManager.LoadScene("Menu");
    }

	IEnumerator playSound()
    {
        //This is a coroutine
        yield return new WaitForSeconds(1f);   //Wait
		gameplayAudios[1].Play();
    }

	public void gameOver()
	{
		GameObject.Find("GameOverText").GetComponent<TextMeshPro>().text = "GAME OVER!";
		playGameOverSound();
		StartCoroutine(goToMenu());

	}
	public void gameWon()
	{
		GameObject.Find("GameOverText").GetComponent<TextMeshPro>().text = "BATLLE WON!";
		playGameWonSound();
		StartCoroutine(goToMenu());
	}


}
