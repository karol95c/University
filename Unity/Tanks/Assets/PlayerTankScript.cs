using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.SceneManagement;
using TMPro;

public class PlayerTankScript : TankScript {

	// Use this for initialization
	int lives = 3;
	public GameObject playerExplodePrefab;
	public Transform playerExplodeSpawn;
	bool shieldActivated;
	GameObject shield;
	bool torchOn;



	Vector3 startPosition;
	void Start () {
		rigidBody = GetComponent<Rigidbody>();
		tankAudio = GetComponents<AudioSource>();
		ammo = 1000;
		ammoReloaded = true;
		startPosition = new Vector3(gameObject.transform.position.x,
			gameObject.transform.position.y, gameObject.transform.position.z);
		shieldActivated = false;
		mainThrust = 300f;
		shield = transform.Find("Shield").gameObject;
		shield.SetActive(false);
		torchOn = false;
		torch = transform.Find("Light").GetComponent<Light>();
		torch.enabled = false;
		nightModeOn = MapScript.isNightModeOn();
		gm = GameObject.Find("GameManager").GetComponent<GameManagerSc>();

	}
	
	// Update is called once per frame
	void Update () {
		Move();
		if (Input.GetKeyUp(KeyCode.F))
		{
			torchManage();
		}
	}
	private void Move()
	{
		rigidBody.freezeRotation = false;
		if (Input.GetKeyUp(KeyCode.Space))
		{
			if(ammoReloaded) StartCoroutine(Wait(0.2f));
		}
		if (Input.GetKey(KeyCode.W))
		{
			if (!tankAudio[0].isPlaying)tankAudio[0].Play();
			rigidBody.AddRelativeForce(-Vector3.forward * mainThrust);
		}
		else if(Input.GetKey(KeyCode.S))
		{
			if (!tankAudio[0].isPlaying)tankAudio[0].Play();
			rigidBody.AddRelativeForce(Vector3.forward * mainThrust);
		}
		if (Input.GetKeyUp(KeyCode.W) || Input.GetKeyUp(KeyCode.S))
		{
			tankAudio[0].Stop();
		}


		float rotationThisFrame = Time.deltaTime * rcsThrust;
		if (Input.GetKey(KeyCode.A))
		{
			if (!tankAudio[1].isPlaying && !tankAudio[0].isPlaying) tankAudio[1].Play();
			transform.Rotate(-Vector3.up * rotationThisFrame);
		}
		else if (Input.GetKey(KeyCode.D))
		{
			if (!tankAudio[1].isPlaying && !tankAudio[0].isPlaying)tankAudio[1].Play();
			transform.Rotate(Vector3.up * rotationThisFrame);
		}
		if (Input.GetKeyUp(KeyCode.A) || Input.GetKeyUp(KeyCode.D))
		{
			tankAudio[1].Stop();
		}

		rigidBody.freezeRotation = false;
	}

	private void torchManage()
	{
		Debug.Log(nightModeOn);
		if (nightModeOn)
		{
			if(torchOn)
			{
				torch.enabled = false;
				torchOn = false;
				Debug.Log("TORCH OFF");
			}
			else
			{
				torch.enabled = true;
				torchOn = true;
				Debug.Log("TORCH ON");
			}
		}
	}
	IEnumerator Wait(float duration)
    {
        //This is a coroutine
		ammoReloaded = false;
		Fire();
        yield return new WaitForSeconds(duration);   //Wait
		ammoReloaded = true;
    }

	IEnumerator makeShield()
    {
        //This is a coroutine
		shield.SetActive(true);
		shieldActivated = true;
        yield return new WaitForSeconds(4f);   //Wait
		shield.SetActive(false);
		shieldActivated = false;
    }

	IEnumerator goToMenu()
    {
        //This is a coroutine
        yield return new WaitForSeconds(4f);   //Wait
		SceneManager.LoadScene("Menu");
    }

	IEnumerator explodedPlayer()
    {
       	GetComponent<Renderer>().enabled = false;
        yield return new WaitForSeconds(1f);   //Wait
		GetComponent<Renderer>().enabled = true;
    }

	public void gotHit()
	{
		Debug.Log("EXPLODE");
		Explode();
		
		--lives;
		StartCoroutine(makeShield());
	}

	private void gameOver()
	{
		GameObject.Find("GameOverText").GetComponent<TextMeshPro>().text = "GAME OVER!";
		gm.playGameplaySound();
		StartCoroutine(goToMenu());

	}

	public void Explode()
	{
		if (!shieldActivated)
		{
			gm.playerDestroyed();
			var playerExplode = (GameObject)Instantiate (
				playerExplodePrefab,
				playerExplodeSpawn.position,
				playerExplodeSpawn.rotation);
			playerExplode.GetComponent<ParticleSystem>().Play();
			playerExplode.GetComponent<AudioSource>().Play();
			Destroy(playerExplode, 1.0f);
			StartCoroutine(explodedPlayer());
			if (lives < 0)
			{
				transform.position = new Vector3(-10f, -10f, -10f);
				gm.gameOver();
			}
			else
			{
				transform.position = startPosition;
				transform.rotation = Quaternion.Euler(0f, 180f, 0f);
			}

		}
	}

}
